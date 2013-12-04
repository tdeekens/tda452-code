import Haste

main :: IO ()
main = do
    Just inp  <- elemById "0-1"
    Just outp <- elemById "0-1"
    onEvent inp OnClick $ \_ -> do
        setProp outp "innerHTML" "boom"
    return ()