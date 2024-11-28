module JSONOps where
import JSON
import TransformOn

instance TransformOn JSON JSON where
  -- TODO: Question 1
  transformOn :: (JSON -> JSON) -> JSON -> JSON
  transformOn f (O objs) = f $ O [(k, transformOn f v) | (k, v) <- objs]
  transformOn f (L items) = f $ L (map (transformOn f) items)        
  transformOn f json = f json

addAll :: TransformOn JSON a => Int -> a -> a
addAll k = transformOn aux
  where
    aux (N x) = N (x + k)
    aux x = x 

negateAll :: TransformOn JSON a => a -> a
negateAll = transformOn aux
  where
    aux (B x) = B (not x)
    aux x = x

filterNull :: TransformOn JSON a => a -> a
filterNull = transformOn aux
  where
    aux (O objs) = O [(k, v) | (k, v) <- objs, v /= Null]
    aux (L items) = L (filter (/= Null) items)
    aux x = x 

instance TransformOn String JSON where
  -- TODO: Question 3
  transformOn :: (String -> String) -> JSON -> JSON
  transformOn f (O objs) = O [(f k, transformOn f v) | (k, v) <- objs]
  transformOn f (L items) = L (map (transformOn f) items)
  transformOn f (S str) = S (f str)
  transformOn _ json = json    

-- Example function for question 3
reverseAllStrings :: TransformOn String a => a -> a
reverseAllStrings = transformOn (reverse :: String -> String)

getN :: JSON -> Maybe Int
getN (N x) = Just x
getN _      = Nothing

queryKey :: String -> JSON -> Maybe JSON
queryKey k (O objs) = lookup k objs
queryKey _ _        = Nothing

getScore :: String -> JSON -> Maybe Int
getScore courseCode student = do
    courses <- queryKey "courses" student
    course <- queryKey courseCode courses
    score <- queryKey "score" course
    getN score 

