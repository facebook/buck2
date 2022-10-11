# 0.3.0

 - Drop dependency on `rand`.

# 0.2.4

 - Yanked due to breaking change.

# 0.2.3

 - Update `rand` from `0.7.3` to `0.8.3`.

# 0.2.2
 
 - Update `rand` from `0.5.5` to `0.7.3`.

# 0.2.1

 - Reduce memory size of `VecList` from 96 bytes to 64. Tradeoff is max capacity is now reduced by
   1 and a very slight performance decrease.

# 0.2.0

 - Change `VecList::retain` to give mutability to entries.

# 0.1.5

 - Add unsafe removal function `VecList::remove_sync`. See its documentation for details.

# 0.1.4

 - Remove unnecessary `Debug` bounds.

# 0.1.3

 - Fix possible overflow when incrementing generation.
 - Fix underflow when calling `pack_to_fit` on an empty `VecList`.

# 0.1.2

 - Make iterator `iter` functions public.

# 0.1.1

 - Iterator optimizations.

# 0.1.0

 - Initial release.
