module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, main_, text)
import Browser.Events as E
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import String exposing (fromInt)

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
  [ { x = 50, y = 350, width = 600}
  , { x = 850, y = 350, width = 600}
  , { x = 1400, y = 250, width = 600}
  , { x = 2000, y = 350, width = 600}
  , { x = 2000, y = 50, width = 600}
  , { x = 2700, y = 350, width = 600}
  , { x = 3200, y = 350, width = 600}
  ]



speed = 2
force = 100

init : () -> ( Model, Cmd Msg )
init _ =
  ( { keys = noKeys
    , player = initPlayer
    , floor = initfloor
    , camera = Camera 0 0 
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
  }

type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }

type Direction = Left | Right

type alias Player =
  { x   : Float
  , y   : Float
  , vx  : Float
  , vy  : Float
  , dir : Direction
  , standingOn : Maybe Floor
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
    

jump : Keys -> Player -> Player
jump keys player =
    if keys.up && player.vy == 0 then { player | vy = 2 } else player

gravity : Float -> Float ->  Player -> Player
gravity dt floor player  =
    { player | vy = if player.y < floor then player.vy - dt/force else 0 }

physics : Float -> Float -> Player  -> Player
physics dt floor player  =
    { player |
        x = player.x + dt * player.vx,
        y = min floor (player.y - dt * player.vy)
    }

walk : Keys -> Player -> Player
walk keys player =
  let {left, right} = keys in
  { player 
    | vx = if keys.left then -1/speed else if keys.right then 1/speed else 0
    , dir = if left then Left else if right then Right else player.dir
  }

initPlayer = 
    { x = 50
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , standingOn = Nothing
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

standingOn : List Floor -> Player ->  Player
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
        |> walk model.keys
        |> physics dt floor
        |> standingOn model.floor

update : Msg -> Model -> Model
update msg model =
  case msg of

    KeyChanged isDown key ->
      { model | keys = updateKeys isDown key model.keys }

    TimeDelta dt ->
      let
        player = step dt model model.player 
      in
      { model 
        | player = player
        , camera = if player.x > 200 then Camera (player.x  - 200 ) model.camera.y else model.camera

      }

    Resized width height ->
        model

    VisibilityChanged _ ->
      { model | keys = noKeys }


view : Model -> Html Msg
view model =
    main_ [ Attr.style "position" "relative"
            , Attr.style "overflow" "hidden"
            , Attr.style "height" "400px"
            , Attr.style "width" "600px"]
        [ div []
          [ viewPlayer model.player model.camera
          , div [] <| List.map (viewFloor model.camera) model.floor 
          ]
        ]

viewPlayer : Player -> Camera -> Html Msg
viewPlayer player camera = 
  let
    {x, y} = { x = player.x - camera.x, y = player.y - camera.y }
  in
  div 
    [ Attr.style "position" "absolute"
    , Attr.style "top" <| (String.fromInt <| round y) ++ "px"
    , Attr.style "left" <| (String.fromInt <| round x) ++ "px"
    ] 
    [ div [Attr.class "h-10 w-10 bg-blue-500 rounded-full absolute top-[-40px]"] []
    , div [Attr.class "h-10 w-5 bg-blue-500 transform -skew-x-12 animate-spin"] []
    ]


viewFloor : Camera -> Floor -> Html Msg
viewFloor camera floor  = 
  let
    {x, y} = { x = floor.x - camera.x, y = floor.y + 40 - camera.y }
  in
  div 
    [ Attr.style "position" "absolute"
    , Attr.style "top" <| (String.fromInt <| round y) ++ "px"
    , Attr.style "left" <| (String.fromInt <| round x) ++ "px"
    ] 
    [ div 
      [ Attr.style "height" "20px"
      , Attr.style "width" ((floor.width |> round |> String.fromInt) ++  "px")
      , Attr.style "background-color" "grey"
      ] []
    ]


