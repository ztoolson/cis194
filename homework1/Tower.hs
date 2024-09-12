-- Disks of different sized are stacked on three pegs; the goal is to get from a starting configuration
-- with all disks stacked on the first peg to an edning configuration with all the disks stacked on the
-- last peg
--
-- The only rules are
-- 1. you may only move 1 disk at a time
-- 2. a larger disk may never be stacked on top of a smaller one
--
--
-- To move _n_ disks from peg a to peg b using peg c as temporary storage,
-- 1. move n-1 disks from a to c using b as temporary storage
-- 2. move the top disk from a to b
-- 3. move n-1 disks from c to b using a as temporary storage

-- example "hanoi" 2 "a" "b" "c" = [("a","c"), ("a","b"), ("c","b")]

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source target _ = [(source, target)]
hanoi n source target temporary =
  hanoi (n - 1) source temporary target -- move n-1 disks to temporary peg
    ++ [(source, target)] -- move top disk to target peg
    ++ hanoi (n - 1) temporary target source -- move n-1 disks from temporary to target peg
