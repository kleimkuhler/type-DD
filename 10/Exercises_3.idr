module Exercises_3

import DataStore

-- 1
testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) ->
            List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec)
    = value :: getValues store | rec

-- 2
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

triangle : Double -> Double -> Shape
triangle = Triangle

rectangle : Double -> Double -> Shape
rectangle = Rectangle

circle : Double -> Shape
circle = Circle

data ShapeView : Shape -> Type where
     STriangle : ShapeView (triangle base height)
     SRectangle : ShapeView (rectangle width height)
     SCircle : ShapeView (circle radius)

shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle

area : Shape -> Double
area shape with (shapeView shape)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius
