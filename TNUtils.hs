module TNUtils where


cond :: [(Bool, a)] -> a
cond ((True, x):_) = x
cond ((False, _):conds) = cond conds

