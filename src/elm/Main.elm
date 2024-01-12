module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, main_, text)
import Browser.Events as E
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import String exposing (fromInt)
import String exposing (left)
import String exposing (right)
import Dict exposing (keys)

-- Pick Up Items to fight with, you hit the exchange button to swap items

--"They made a calf in Horeb, and worshiped a molten image."



--"Thus they exchanged their glory for an image of a bull that eats grass."
type Screen = Menu Float | Game

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrameDelta TimeDelta
    , E.onVisibilityChange VisibilityChanged
    ]

initfloor = 
  [ { x = 50, y = 350, width = 700}
  , { x = 850, y = 350, width = 600}
  , { x = 1400, y = 250, width = 600}
  , { x = 2000, y = 350, width = 600}
  , { x = 2000, y = 50, width = 600}
  , { x = 2700, y = 350, width = 600}
  , { x = 3200, y = 350, width = 600}
  ]




force = 100

init : () -> ( Model, Cmd Msg )
init _ =
  ( { keys = noKeys
    , player = initPlayer
    , camera = Camera 0 0 
    , floor = initfloor
    , boss = initBoss
    , screen = Game --Menu -1
    }
  , Cmd.batch
      [ 
      ]
  )

type alias Model =
  { keys : Keys
  , player : Player
  , camera : Camera
  , floor : List Floor
  , boss : Boss
  , screen : Screen
  }


type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }



type Direction = Left | Right

type alias SwordData = 
  { strength : Int
  , speed : Int
  }

type CoolDown = CoolDown Int
type AnimationTime = AnimationTime Int



type Weapon = 
  Sword SwordData

type WeaponControl = Holding Weapon CoolDown| Attacking Weapon AnimationTime

type alias Player =
  { x   : Float
  , y   : Float
  , vx  : Float
  , vy  : Float
  , dir : Direction
  , standingOn : Maybe Floor
  , weaponControl : WeaponControl
  }

type alias Boss = 
  { x   : Float
  , y   : Float
  , vx  : Float
  , vy  : Float
  , dir : Direction
  , standingOn : Maybe Floor
  , moveTo : Maybe (Float, Float)
  }

type alias Camera = 
  { x : Float
  , y : Float
  }

type alias Floor = 
  { x : Float
  , y : Float
  , width : Float
  }

type Collided = RightCollide CollisionData | LeftCollide CollisionData | TopCollide CollisionData | BottomCollide CollisionData

type alias CollisionData = 
  { x : Float
  , y : Float
  , w : Float
  , h : Float
  }

checkCollisions : CollisionData -> List CollisionData -> List Collided
checkCollisions model nodesToCheck =
  let {x, y, w, h } = model in
  nodesToCheck 
    |> List.filter (\data ->
        let {bx, by, bw, bh} = { bx = data.x, by = data.y, bw = data.w, bh = data.h} in 
        (x < bx + bw && x + w > bx) && (y < by + bh && y + h > by)
      ) 
    |> List.map (\item ->
        let (cx, cy) = (item.x + (item.w / 2), item.y + (item.h / 2)) in
        if x + w > cx then
            RightCollide item
        else if x < cx then
            LeftCollide item
        else if y < cy then
            TopCollide item
        else
            BottomCollide item
      )
    


--jump : Keys -> Player -> Player
jump keys player =
    if keys.up && player.vy == 0 then { player | vy = 2 } else player

--gravity : Float -> Float ->  Player -> Player
gravity dt floor player  =
    { player | vy = if player.y < floor then player.vy - dt/force else 0 }

--physics : Float -> Float -> Player  -> Player
physics dt floor player  =
    { player |
        x = player.x + dt * player.vx,
        y = min floor (player.y - dt * player.vy)
    }

--walk : Keys -> Player -> Player
walk keys speed player  =
  let {left, right} = keys in
  { player 
    | vx = if keys.left then -1/speed else if keys.right then 1/speed else 0
    , dir = if left then Left else if right then Right else player.dir
  }

initPlayer : Player
initPlayer = 
    { x = 50
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , standingOn = Nothing
    , weaponControl = Holding (Sword {strength = 5, speed = 10}) (CoolDown 0)
    }

initBoss : Boss
initBoss =     
    { x = 200
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , standingOn = Nothing
    , moveTo = Just (10, 0)
    }


noKeys : Keys
noKeys =
  Keys False False False False False


type Msg
  = KeyChanged Bool String
  | TimeDelta Float
  | Resized Float Float
  | VisibilityChanged E.Visibility

updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> keys


standingOn  floor player = 
  checkCollisions 
    {x = player.x, y = player.y, w = 50, h = 50}
    (List.map ( \ i -> {x = i.x, y = i.y, w = i.width, h = 20})  floor)
    |> ( \ n -> 
      case n of 
        [] -> Nothing
        TopCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        RightCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        LeftCollide c::_ -> 
          Just <| Floor c.x c.y c.w
        BottomCollide c::_ -> 
          Just <| Floor c.x c.y c.w
      )
    |> ( \ g-> { player | standingOn = g } )

attack : Keys -> Player -> Player
attack keys player = 
  case player.weaponControl of 
    Holding (Sword data) (CoolDown 0) -> 
      if keys.space then
        { player | weaponControl = Attacking (Sword data) (AnimationTime <| data.speed) }
      else 
        player

    Holding (Sword data) (CoolDown cd) -> 
      { player | weaponControl = Holding (Sword data) (CoolDown <| cd - 1) }
      

    Attacking (Sword data) (AnimationTime t) ->
      if t == 0 then 
        { player | weaponControl = Holding (Sword data) (CoolDown <| data.speed) }
      else 
        { player | weaponControl = Attacking (Sword data) (AnimationTime <| t - 1) }


step : Float -> Model  -> Player -> Player
step dt model player =
  let
    floor = case player.standingOn of 
      Just c -> c.y 
      Nothing -> 99999
  in
    player
        |> gravity dt floor
        |> jump model.keys
        |> walk model.keys 2
        |> physics dt floor
        |> standingOn model.floor
        |> attack model.keys


keysleft = { up  = False
       , left  = True
       , down  = False
       , right  = False
       , space = False
       }

keysright = { up  = False
       , left  = False
       , down  = False
       , right  = True
       , space = False
       }
bossAI : Model -> Boss -> Boss
bossAI model boss = 
  case (model.player.standingOn, boss.standingOn) of
    (Just a, Just b) -> 
      if  a == b then 
        { boss | moveTo = Just (model.player.x, boss.y) }
      else 
        boss

    _ -> 
      boss

stepBoss : Float -> Model  -> Boss -> Boss
stepBoss dt model boss =
  let
    floor = case boss.standingOn of 
      Just c -> c.y 
      Nothing -> 99999

    keys = 
      case boss.moveTo of 
        Just (x, y) -> 
          if boss.x > x then 
            keysleft
          else if boss.x < x then 
            keysright
          else
            noKeys

        Nothing -> 
          noKeys
            
  in
    boss
        |> gravity dt floor
        |> jump keys
        |> walk keys 4
        |> physics dt floor
        |> standingOn model.floor
        |> bossAI model

update : Msg -> Model -> Model
update msg model =
  case msg of

    KeyChanged isDown key ->
      case model.screen of 
        Menu i ->
          { model | screen = Menu 1 }
            
        Game -> 
          { model | keys = updateKeys isDown key model.keys }

    TimeDelta dt ->
      case model.screen of 
        Menu i -> 
          {model | 
            screen = 
              if i > 100 then Game 
              else if i /= -1 then Menu (i + 0.4) 
              else Menu -1 

          }

        Game -> 
          let
            player =  step dt model model.player 
            boss = stepBoss dt model model.boss 
          in
          { model 
            | player = player
            , boss = boss
            , camera = if player.x > 200 then Camera (player.x  - 200 ) model.camera.y else model.camera
          }
          

    Resized width height ->
        model

    VisibilityChanged _ ->
      { model | keys = noKeys }


-- I have taken this time, the exchange is complete.

view : Model -> Html Msg
view model =
    main_ 
      [ Attr.style "position" "relative"
      , Attr.style "overflow" "hidden"
      , Attr.style "height" "400px"
      , Attr.style "width" "600px"]
      (case model.screen of 
        Menu i -> 
          [ Html.p [Attr.class "p-2 w-48"] [text "They made a calf in Horeb, and worshiped a molten image."]
          , Html.p [Attr.class "p-2 w-48"] [text "Thus they exchanged their glory for an image of a bull that eats grass."]
          , viewPlayer initPlayer (Camera -180 -350)
          , viewHead 200 (50 +  i) 100
          , Html.p [Attr.class "p-8"] [text "press any key to start..."]
          ]
        Game -> 
          [ viewPlayer model.player model.camera
          , case model.player.weaponControl of
              Attacking (Sword _) (AnimationTime _) ->
                viewSword model.player model.camera
              _ -> div [] []
          , viewBoss model.boss model.camera
          , div [] <| List.map (viewFloor model.camera) model.floor 
          ]
      )

position : Float -> Float -> List (Html.Attribute Msg)
position x y = 
  [ Attr.style "position" "absolute"
  , Attr.style "top" <| (String.fromInt <| round y) ++ "px"
  , Attr.style "left" <| (String.fromInt <| round x) ++ "px"
  ] 

viewSword player camera = 
  let
    {x, y} = { x = player.x - camera.x, y = player.y - camera.y }
    angle = case player.dir of 
      Left -> "rotate-90 -mt-12 -ml-12"
      Right -> "-rotate-90 -mt-12 ml-8"

    
  in
  div 
    (position (x) y)
    [ div [Attr.class "animate-pulse"] [div [Attr.class <| "triangle transform " ++ angle] []]
    ]

viewPlayer : Player -> Camera -> Html Msg
viewPlayer player camera = 
  let
    {x, y} = { x = player.x - camera.x, y = player.y - camera.y }
  in
  div 
    (position x y)
    [ div [Attr.class "h-10 w-10 bg-blue-500 rounded-full absolute top-[-40px]"] []
    , div [Attr.class "h-10 w-5 bg-blue-500 transform -skew-x-12 "] []
    ]

viewHead x y sizein  =
  let (size, sizeBy) = (String.fromInt sizein, String.fromInt (sizein * 2)) in
  div 
    (position x y) 
    [ div [Attr.class "animate-pulse"]
      [ div [Attr.class <| "triangle transform rotate-45 absolute top-[50px]"] []
      , div [Attr.class <| "triangle transform -rotate-45 absolute top-[50px] left-[50px]"] []
      , div [Attr.class <| "pentagon absolute top-[100px]"] []
      ]
    ]


viewBoss : Boss -> Camera -> Html Msg
viewBoss bos camera = 
  let
    {x, y} = { x = bos.x - camera.x, y = bos.y - camera.y }
  in
  div 
    (position x y)
    [ div [Attr.class "h-10 w-10 bg-red-500 rounded-full absolute top-[-40px]"] []
    , div [Attr.class "h-10 w-5 bg-red-500 transform -skew-x-12 animate-spin"] []
    ]


viewFloor : Camera -> Floor -> Html Msg
viewFloor camera floor  = 
  let
    {x, y} = { x = floor.x - camera.x, y = floor.y + 40 - camera.y }
  in
  div 
    (position x y)
    [ div 
      [ Attr.style "height" "20px"
      , Attr.style "width" ((floor.width |> round |> String.fromInt) ++  "px")
      , Attr.style "background-color" "grey"
      ] 
      []
    ]

 
