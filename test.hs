correctDollarSign :: String -> String
correctDollarSign s = helper s 0
  where helper "" _ = ""
        helper ('$':'$':x) n = '$':(helper ('$':x) 3)
        helper ('$':x) 0 = '\\':'(':(helper x 1)
        helper ('$':x) 1 = '\\':')':(helper x 0)
        helper ('$':x) 2 = '$':(helper x 0)
        helper (a:x) 3 = a:(helper x 2)
        helper (a:x) n = a:(helper x n)
