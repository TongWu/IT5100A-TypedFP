module JSONOps where
import JSON
import TransformOn


instance TransformOn JSON JSON where
  transformOn :: (JSON -> JSON) -> JSON -> JSON
  transformOn f (O ls) = f $ O (f' <$> ls) where
    f' (k, v) = (k, transformOn f v)
  transformOn f (L ls) = f $ L (transformOn f <$> ls)
  transformOn f x = f x

addAll :: TransformOn JSON a => Int -> a -> a
addAll k = transformOn aux where
  aux (N x) = N (x + k)
  aux x = x

negateAll :: TransformOn JSON a => a -> a
negateAll = transformOn aux where
  aux (B x) = B (not x)
  aux x = x

filterNull :: TransformOn JSON a => a -> a
filterNull = transformOn aux where
  aux (O ls) = O $ filter f ls
  aux (L ls) = L $ filter (/= Null) ls
  aux x = x
  f (k, v) = v /= Null


instance TransformOn String JSON where
  transformOn f (O ls) = O (f' <$> ls) where
    f' (k, v) = (f k, transformOn f v)
  transformOn f (L ls) = L (transformOn f <$> ls)
  transformOn f (S s) = S (f s)
  transformOn f x = x


-- Example function for question 3
reverseAllStrings :: TransformOn String a => a -> a
reverseAllStrings = transformOn (reverse :: String -> String)

getN :: JSON -> Maybe Int
getN (N x) = Just x
getN _     = Nothing

queryKey :: String -> JSON -> Maybe JSON
queryKey k (O ls) = lookup k ls
queryKey _ _      = Nothing

getScore :: String -> JSON -> Maybe Int
getScore course_code student = do
  courses <- queryKey "courses" student
  course  <- queryKey course_code courses
  score   <- queryKey "score" course
  getN score

