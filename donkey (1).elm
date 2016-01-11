import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window




type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


donkey : Model
donkey =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }




--update : (Float, Keys) -> Model -> Model
update (dt, t) donkey =
  donkey
    |> gravity dt
    |> jump t
    |> walk t
    |> physics dt


--jump : Keys -> Model -> Model
jump t donkey =

  
  if t > 0  && donkey.vy == 0 then      
  
      { donkey | vy = 8.0 * abs(sin(t/20)) }
    
  else
      donkey
      

    


gravity : Float -> Model -> Model
gravity dt donkey =
  { donkey |
      vy = if donkey.y > 0 then donkey.vy - dt/20 else 0
  }


physics : Float -> Model -> Model
physics dt donkey =
  { donkey |
      x = donkey.x + dt *2* donkey.vx,    
      y = max 0 (donkey.y) + dt * (donkey.vy)
      
  }


--walk : Keys -> Model -> Model
walk t donkey =
  { donkey |
      vx = 0.5 * sin(t) ,
      dir = if sin(t) > 0
            then Left
            else Right
      {--speed = if (t > 0)
              then if (t>3)
                   then if (t>6)
                        then 1
                   else 0.5
              else 0 --}
              
              

        
            
  }


-- VIEW

--view : (Int, Int) -> Model -> Element
view (w',h') donkey =
  let
    (w,h) = (toFloat w', toFloat h')
    p = if donkey.y > 400
        then donkey.y
        else 0
    verb =
      if donkey.y > 5 then
          "spin"
          
      else if donkey.y > 0 then
          "jump"

      else if donkey.vx /= 0 then
          "walk"

      else
          "stand"

    dir =
      case donkey.dir of
        Left -> "left"
        Right -> "right"

    donkeyImage =
      image 180 80 "http://i.imgur.com/Mr6aUsu.jpg"
    
    volcano =
      filled brown (rect 4000 20)



    groundY = 62 - h/2

    position =
      (donkey.x, donkey.y + groundY)
  in
    collage w' h' 
      [ rect w h
          |> filled (white)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , donkeyImage
          |> toForm
          |> move position
          |> move ( 130 ,35 )
          |> rotate (degrees 1.5*p)
          
      , volcano    
          
          |> move ((h/10)-340, 130 - h/2)

      , circle 30
        |> filled yellow
      ,  show "As you can see, the donkey is eternally chasing this carrot. He has an abundance of volition but a lack of faculty"
          |> toForm
          |> move (0, 300)
      ,  show "The donkey wants to jump over the fence. At first this is impossible because his legs are stiff, his situation is similar to a game that is too hard"
          |> toForm
          |> move (0,200)
      ,  show "But as he gains momentum he reaches a point where he can hop over the fence, and it is a fun challenge"
          |> toForm
          |> move (0,150)
      ,  show "After few jumps he has mastered the fence, and he can even jump over the sun and now the challenge is too easy. "
          |> toForm
          |> move (0,100)
          
      ]



main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update donkey input) 






input =
  let
    delta = Signal.map (\t -> t/8) (fps 90)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta (every second))
    