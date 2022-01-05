-- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyRedeemer type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import Control.Monad (void)
import Ledger (Address, ScriptContext)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))
-- | BoilerPlate: 
-- | STEP 1: DEFINE DATUMS, DATUM AND REDEEMER
    -- | These are the data script and redeemer types. We are using an integer
    --   value for both, but you should define your own types.

    --   TYPES defined: MyDatum - MyRedeemer
    newtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    -- | makeLift: it allows to use datatype in haskell and plutus core
    PlutusTx.makeLift ''MyDatum
    -- Equivalent: Public class myDatum extends data{
    -- public myDatum(Integer){
    --  this.i = i;   
    -- }
    --}
    newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    PlutusTx.makeLift ''MyRedeemer
-- | END STEP1  // 

-- | STEP 2: Define validator script
    -- | This method is the spending validator (which gets lifted to
    --   its on-chain representation).
    -- VALIDATION SCRIPT: Takes Datum , Redeemer , Validation context, and bool to see if the transaction succeed or not
    validateSpend :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
    -- if 
    validateSpend _myDataValue _myRedeemerValue _ = error () -- Please provide an implementation.
-- | END STEP 2 // 

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

-- | STEP 3: define redeemer and datum in starter instance
    data Starter
    instance Scripts.ValidatorTypes Starter where
        type instance RedeemerType Starter = MyRedeemer
        type instance DatumType Starter = MyDatum
-- | END STEP 3 // 

-- | STEP 4: Use starter instance script
    -- | The script instance is the compiled validator (ready to go onto the chain)
    starterInstance :: Scripts.TypedValidator Starter -- Starter: script type
    starterInstance = Scripts.mkTypedValidator @Starter
        $$(PlutusTx.compile [|| validateSpend ||]) -- validateSpend: Validation Script
        $$(PlutusTx.compile [|| wrap ||]) where
            wrap = Scripts.wrapValidator @MyDatum @MyRedeemer -- Define datum and redeemer in wrap variable
---- | END STEP 4// 

-- | STEP 5: define Schema
-- | The schema of the contract, with two endpoints.
    -- way the user interacts with blockchain
    type Schema =
            Endpoint "publish" (Integer, Value) --2 valuesInteger, Value)
            .\/ Endpoint "redeem" Integer
---- | END STEP 5//        
 -- | STEP 6: endpoints function
    -- contract endpoint, use contract schema
    -- | STEP 6.1: define contract
        contract :: AsContractError e => Contract () Schema e ()
        contract = selectList [publish, redeem] -- this says, if user selects publis, then will use publish function, otherwise will use redeem function
    ---- | END STEP 6.1// 
    -- | STEP 6.2: define publish contract endpoint (Logic of smart contract)
        -- | The "publish" contract endpoint.
        -- | publish word is defined in schema
        publish :: AsContractError e => Promise () Schema e ()
        publish = endpoint @"publish" $ \(i, lockedFunds) -> do -- Endpoint "publish" (Integer, Value) --2 (valuesInteger -i-, Value-lockedFunds-) in shcema
            let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
            void $ submitTxConstraints starterInstance tx --send transaction to cardano's blockchain
    ---- | END STEP 6.2//   
    -- | STEP 6.3: define redeem contract endpoint (Logic of smart contract)
        -- | The "redeem" contract endpoint.
        -- contract endpoint, use contract schema
        -- | redeem word is defined in schema   
        redeem :: AsContractError e => Promise () Schema e ()
        redeem = endpoint @"redeem" $ \myRedeemerValue -> do
            unspentOutputs <- utxosAt contractAddress
            let redeemer = MyRedeemer myRedeemerValue
                tx       = collectFromScript unspentOutputs redeemer
            void $ submitTxConstraintsSpending starterInstance unspentOutputs tx --send transaction to cardano's blockchain
    ---- | END STEP 6.3//    
    -- | STEP 6.4: define endpoints and use contract
        -- | is how plutus knows what to show to user - use contract 
        endpoints :: AsContractError e => Contract () Schema e ()
        endpoints = contract
    ---- | END STEP 6.4//      
 ---- | END STEP 6//  
-- | schema of this specific project
mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
