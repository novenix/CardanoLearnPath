import Data.Text qualified as T
import Playground.Contract
import Language.Plutus.Contract hiding (when)
import Plutus.Contract
import PlutusTx.Prelude
import Prelude qualified as Haskell


-- | ex in java
-- | public Contract<EmptySchema,t.text,void> hello(){
 --        return logInfo(@String "Hello,world");
-- }

-- | A 'Contract' that logs a message.
-- | function definition
hello :: Contract () BlockchainActions T.Text ()
-- | function implementation
hello = logInfo @Haskell.String "Hello, world"

-- | this is how plutus knows what to show in UI
-- |its saying our endpoints is returning a contract (hello)
endpoints :: Contract () BlockchainActions T.Text ()
endpoints = hello

-- 'mkSchemaDefinitions' doesn't work with 'EmptySchema'
-- (that is, with 0 endpoints) so we define a
-- dummy schema type with 1 endpoint to make it compile.
-- TODO: Repair 'mkSchemaDefinitions'
--type DummySchema = Endpoint "dummy" ()

mkSchemaDefinitions ''BlockchainActions

$(mkKnownCurrencies [])
