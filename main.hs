import Web.Scotty

main :: IO ()
main = scotty 4321 $ do
  get "/" $ html "Hello, world!"
