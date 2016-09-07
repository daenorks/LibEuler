-- pb 27 -----it's ezzzzzzzzzzzzzzzzzzz
ulamDiags 1 = 1
ulamDiags n = ((ulamDiags (n-1)) + 2*(truncate((n-2)/4) + 1))