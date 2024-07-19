type Bit = Int

bit2int :: [Bit] -> Int
bit2int bits= foldr (\(b,idx) acc -> b * (2^idx) + acc) 0 pairBits
    where pairBits = zip bits [length bits -1, length bits -2..0]
