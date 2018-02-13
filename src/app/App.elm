module App exposing (..)

import Html exposing (Html, Attribute, button, div, h1, text, img, input, a, ul, li)
import Html.Attributes exposing (src, title, class, id, type_, href)
import Html.Events exposing (on, onWithOptions, onClick)
import Navigation exposing (Location)
import Translation.App as Translation
import UrlParser as UP
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Decode.Pipeline as JDP
import Date exposing (Date)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config, route : Route, location : Location,  mysteries : List Mystery }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias Mystery =
    { id : String, drivers : List Driver, cars : List Car, mysteriesSolveTags : List MysterySolveTag }


type alias Driver =
    { id : Int, mysteryId : String, name : String, birthDate : Date, gender : String, race : String, email : String, address : String, smartphones : List Smartphone, wallets : List Wallet, keyNames : List KeyName }


type alias Smartphone =
    { id : Int, driverId : Int, contacts : List Contact, apps : List App, messages : List Message, toDoItems : List ToDoItem, browserHistoryEntries : List BrowserHistoryEntry, callHistoryEntries : List CallHistoryEntry }


type alias Contact =
    { id : Int, smartphoneId : Int, name : String }


type alias App =
    { id : Int, smartphoneId : Int, name : String }


type alias Message =
    { id : Int, smartphoneId : Int, sender : String, receiver : String, sendDateTime : Date, message : String, seenByRecipient : Bool }


type alias ToDoItem =
    { id : Int, smartphoneId : Int, description : String, isDone : Bool, addedDate : Date }


type alias BrowserHistoryEntry =
    { id : Int, smartphoneId : Int, url : String, visitDateTime : Date }


type alias CallHistoryEntry =
    { id : Int, smartphoneId : Int, phoneNumber : String, callDateTime : Date, isIncoming : Bool, isOutgoing : Bool, isSuccessful : Bool }


type alias Wallet =
    { id : Int, driverId : Int, cashCurrency : String, cashAmount : Float, bloodTypeCard : String, creditCards : List CreditCard }


type alias CreditCard =
    { id : Int, walletId : Int, brand : String, validUntilDate : Date, name : String, number : String }


type alias KeyName =
    { id : Int, driverId : Int, name : String }


type alias Car =
    { id : Int, mysteryId : String, carType : String, dashBoardOrnament : String, mirrorOrnament : String, licensePlateCountry : String, licensePlate : String, gloveCompartmentItems : List GloveCompartmentItem, trunkContentItem : List TrunkContentItem, cupHolderContentItems : List CupHolderContentItem, damageEntries : List DamageEntry, diaryEntries : List DiaryEntry }


type alias GloveCompartmentItem =
    { id : Int, carId : Int, description : String }


type alias TrunkContentItem =
    { id : Int, carId : Int, description : String }


type alias CupHolderContentItem =
    { id : Int, carId : Int, description : String }


type alias DamageEntry =
    { id : Int, carId : Int, description : String }


type alias DiaryEntry =
    { id : Int, carId : Int, pageNumber : Int, pageDate : Date, pageTitle : String, pageBody : String }


type alias MysterySolveTag =
    { mysteryId : String, solveTagId : Int, solveTag : SolveTag }


type alias SolveTag =
    { id : Int, description : String }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            parseLocation location
    in
        ( { config = { version = flags.version, baseURL = flags.baseURL }, route = currentRoute, location = location, mysteries = [] }, Cmd.none )



-- DATA


endpointMysteries : String
endpointMysteries =
    "/mysteries"


queryMysteries : String
queryMysteries =
    "?select=id,driver:drivers(*,smartphone:smartphones(*,contacts(*),apps(*),messages(*),toDo_items(*),browser_history_entries(*),call_history_entries(*)),wallet:wallets(*,credit_cards(*)),key_names(*)),car:cars(*,glove_compartment_items(*),trunk_content_items(*),cupHolder_content_items(*),damage_entries(*),diary_entries(*)),mysteries_solve_tags(*,solve_tag:solve_tags(*))"



-- DECODERS


decodeMystery : JD.Decoder Mystery
decodeMystery =
    JDP.decode Mystery
        |> JDP.required "id" JD.string
        |> JDP.required "driver" decodeDrivers
        |> JDP.required "car" decodeCars
        |> JDP.required "mysteries_solve_tags" decodeMysteriesSolveTags


decodeMysteries : JD.Decoder (List Mystery)
decodeMysteries =
    JD.list decodeMystery


decodeDriver : JD.Decoder Driver
decodeDriver =
    JDP.decode Driver
        |> JDP.required "id" JD.int
        |> JDP.required "mystery_id" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "birth_date" JDE.date
        |> JDP.required "gender" JD.string
        |> JDP.required "race" JD.string
        |> JDP.required "email" JD.string
        |> JDP.required "address" JD.string
        |> JDP.required "smartphone" decodeSmartphones
        |> JDP.required "wallet" decodeWallets
        |> JDP.required "key_names" decodeKeyNames


decodeDrivers : JD.Decoder (List Driver)
decodeDrivers =
    JD.list decodeDriver


decodeSmartphone : JD.Decoder Smartphone
decodeSmartphone =
    JDP.decode Smartphone
        |> JDP.required "id" JD.int
        |> JDP.required "driver_id" JD.int
        |> JDP.required "contacts" decodeContacts
        |> JDP.required "apps" decodeApps
        |> JDP.required "messages" decodeMessages
        |> JDP.required "toDo_items" decodeToDoItems
        |> JDP.required "browser_history_entries" decodeBrowserHistoryEntries
        |> JDP.required "call_history_entries" decodeCallHistoryEntries


decodeSmartphones : JD.Decoder (List Smartphone)
decodeSmartphones =
    JD.list decodeSmartphone


decodeContact : JD.Decoder Contact
decodeContact =
    JDP.decode Contact
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "name" JD.string


decodeContacts : JD.Decoder (List Contact)
decodeContacts =
    JD.list decodeContact


decodeApp : JD.Decoder App
decodeApp =
    JDP.decode App
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "name" JD.string


decodeApps : JD.Decoder (List App)
decodeApps =
    JD.list decodeApp


decodeMessage : JD.Decoder Message
decodeMessage =
    JDP.decode Message
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "sender" JD.string
        |> JDP.required "receiver" JD.string
        |> JDP.required "send_date_time" JDE.date
        |> JDP.required "message" JD.string
        |> JDP.required "seen_by_recipient" JD.bool


decodeMessages : JD.Decoder (List Message)
decodeMessages =
    JD.list decodeMessage


decodeToDoItem : JD.Decoder ToDoItem
decodeToDoItem =
    JDP.decode ToDoItem
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "description" JD.string
        |> JDP.required "is_done" JD.bool
        |> JDP.required "added_date" JDE.date


decodeToDoItems : JD.Decoder (List ToDoItem)
decodeToDoItems =
    JD.list decodeToDoItem


decodeBrowserHistoryEntry : JD.Decoder BrowserHistoryEntry
decodeBrowserHistoryEntry =
    JDP.decode BrowserHistoryEntry
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "url" JD.string
        |> JDP.required "visit_date_time" JDE.date


decodeBrowserHistoryEntries : JD.Decoder (List BrowserHistoryEntry)
decodeBrowserHistoryEntries =
    JD.list decodeBrowserHistoryEntry


decodeCallHistoryEntry : JD.Decoder CallHistoryEntry
decodeCallHistoryEntry =
    JDP.decode CallHistoryEntry
        |> JDP.required "id" JD.int
        |> JDP.required "smartphone_id" JD.int
        |> JDP.required "phone_number" JD.string
        |> JDP.required "call_date_time" JDE.date
        |> JDP.required "is_incoming" JD.bool
        |> JDP.required "is_outgoing" JD.bool
        |> JDP.required "is_successful" JD.bool


decodeCallHistoryEntries : JD.Decoder (List CallHistoryEntry)
decodeCallHistoryEntries =
    JD.list decodeCallHistoryEntry


decodeWallet : JD.Decoder Wallet
decodeWallet =
    JDP.decode Wallet
        |> JDP.required "id" JD.int
        |> JDP.required "driver_id" JD.int
        |> JDP.required "cash_currency" JD.string
        |> JDP.required "cash_amount" JD.float
        |> JDP.required "blood_type_card" JD.string
        |> JDP.required "credit_cards" decodeCreditCards


decodeWallets : JD.Decoder (List Wallet)
decodeWallets =
    JD.list decodeWallet


decodeCreditCard : JD.Decoder CreditCard
decodeCreditCard =
    JDP.decode CreditCard
        |> JDP.required "id" JD.int
        |> JDP.required "wallet_id" JD.int
        |> JDP.required "brand" JD.string
        |> JDP.required "valid_until_date" JDE.date
        |> JDP.required "name" JD.string
        |> JDP.required "number" JD.string


decodeCreditCards : JD.Decoder (List CreditCard)
decodeCreditCards =
    JD.list decodeCreditCard


decodeKeyName : JD.Decoder KeyName
decodeKeyName =
    JDP.decode KeyName
        |> JDP.required "id" JD.int
        |> JDP.required "driver_id" JD.int
        |> JDP.required "name" JD.string


decodeKeyNames : JD.Decoder (List KeyName)
decodeKeyNames =
    JD.list decodeKeyName


decodeCar : JD.Decoder Car
decodeCar =
    JDP.decode Car
        |> JDP.required "id" JD.int
        |> JDP.required "mystery_id" JD.string
        |> JDP.required "type" JD.string
        |> JDP.required "dashboard_ornament" JD.string
        |> JDP.required "mirror_ornament" JD.string
        |> JDP.required "license_plate_country" JD.string
        |> JDP.required "license_plate" JD.string
        |> JDP.required "glove_compartment_items" decodeGloveCompartmentItems
        |> JDP.required "trunk_content_items" decodeTrunkContentItems
        |> JDP.required "cupHolder_content_items" decodeCupHolderContentItems
        |> JDP.required "damage_entries" decodeDamageEntries
        |> JDP.required "diary_entries" decodeDiaryEntries


decodeCars : JD.Decoder (List Car)
decodeCars =
    JD.list decodeCar


decodeGloveCompartmentItem : JD.Decoder GloveCompartmentItem
decodeGloveCompartmentItem =
    JDP.decode GloveCompartmentItem
        |> JDP.required "id" JD.int
        |> JDP.required "car_id" JD.int
        |> JDP.required "description" JD.string


decodeGloveCompartmentItems : JD.Decoder (List GloveCompartmentItem)
decodeGloveCompartmentItems =
    JD.list decodeGloveCompartmentItem


decodeTrunkContentItem : JD.Decoder TrunkContentItem
decodeTrunkContentItem =
    JDP.decode TrunkContentItem
        |> JDP.required "id" JD.int
        |> JDP.required "car_id" JD.int
        |> JDP.required "description" JD.string


decodeTrunkContentItems : JD.Decoder (List TrunkContentItem)
decodeTrunkContentItems =
    JD.list decodeTrunkContentItem


decodeCupHolderContentItem : JD.Decoder CupHolderContentItem
decodeCupHolderContentItem =
    JDP.decode CupHolderContentItem
        |> JDP.required "id" JD.int
        |> JDP.required "car_id" JD.int
        |> JDP.required "description" JD.string


decodeCupHolderContentItems : JD.Decoder (List CupHolderContentItem)
decodeCupHolderContentItems =
    JD.list decodeCupHolderContentItem


decodeDamageEntry : JD.Decoder DamageEntry
decodeDamageEntry =
    JDP.decode DamageEntry
        |> JDP.required "id" JD.int
        |> JDP.required "car_id" JD.int
        |> JDP.required "description" JD.string


decodeDamageEntries : JD.Decoder (List DamageEntry)
decodeDamageEntries =
    JD.list decodeCupHolderContentItem


decodeDiaryEntry : JD.Decoder DiaryEntry
decodeDiaryEntry =
    JDP.decode DiaryEntry
        |> JDP.required "id" JD.int
        |> JDP.required "car_id" JD.int
        |> JDP.required "page_number" JD.int
        |> JDP.required "page_date" JDE.date
        |> JDP.required "page_title" JD.string
        |> JDP.required "page_body" JD.string


decodeDiaryEntries : JD.Decoder (List DiaryEntry)
decodeDiaryEntries =
    JD.list decodeDiaryEntry


decodeMysterySolveTag : JD.Decoder MysterySolveTag
decodeMysterySolveTag =
    JDP.decode MysterySolveTag
        |> JDP.required "mystery_id" JD.string
        |> JDP.required "solve_tag_id" JD.int
        |> JDP.required "solve_tag" decodeSolveTag


decodeMysteriesSolveTags : JD.Decoder (List MysterySolveTag)
decodeMysteriesSolveTags =
    JD.list decodeMysterySolveTag


decodeSolveTag : JD.Decoder SolveTag
decodeSolveTag =
    JDP.decode SolveTag
        |> JDP.required "id" JD.int
        |> JDP.required "description" JD.string



-- ROUTING
type Route
    = RouteLanding
    | RouteMystery
    | RouteDriver
    | RouteSmartphone
    | RouteContacts
    | RouteApps
    | RouteMessages
    | RouteToDoList
    | RouteBrowserHistory
    | RouteCallHistory
    | RouteWallet
    | RouteCreditCards
    | RouteKeys
    | RouteCar
    | RouteGloveCompartment
    | RouteTrunk
    | RouteCupHolder
    | RouteDamage
    | RouteDiary
    | RouteTags
    | RouteNotFound

matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RouteLanding UP.top
        , UP.map RouteMystery (UP.s Translation.locationMystery)
        , UP.map RouteDriver (UP.s Translation.locationDriver)
        , UP.map RouteSmartphone (UP.s Translation.locationSmartphone)
        , UP.map RouteContacts (UP.s Translation.locationContacts)
        , UP.map RouteApps (UP.s Translation.locationApps)
        , UP.map RouteMessages (UP.s Translation.locationMessages)
        , UP.map RouteToDoList (UP.s Translation.locationToDoList)
        , UP.map RouteBrowserHistory (UP.s Translation.locationBrowserHistory)
        , UP.map RouteCallHistory (UP.s Translation.locationCallHistory)
        , UP.map RouteWallet (UP.s Translation.locationWallet)
        , UP.map RouteCreditCards (UP.s Translation.locationCreditCards)
        , UP.map RouteKeys (UP.s Translation.locationKeys)
        , UP.map RouteCar (UP.s Translation.locationCar)
        , UP.map RouteGloveCompartment (UP.s Translation.locationGloveCompartment)
        , UP.map RouteTrunk (UP.s Translation.locationTrunk)
        , UP.map RouteCupHolder (UP.s Translation.locationCupHolder)
        , UP.map RouteDamage (UP.s Translation.locationDamage)
        , UP.map RouteDiary (UP.s Translation.locationDiary)
        , UP.map RouteTags (UP.s Translation.locationTags)
        ]


parseLocation : Location -> Route
parseLocation location =
    case UP.parseHash matchers location of
        Just route ->
            route

        Nothing ->
            RouteNotFound



-- UPDATE


type Msg
    = ChangePath String
    | LocationChange Location
    | RetrieveMysteries
    | NewMysteries (Result Http.Error (List Mystery))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangePath path ->
            ( model, Navigation.newUrl path )

        LocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        RetrieveMysteries ->
            ( model, retrieveMysteries model )

        NewMysteries (Ok newMysteries) ->
            ( { model | mysteries = newMysteries }, Cmd.none )

        NewMysteries (Err _) ->
            ( model, Cmd.none )


retrieveMysteries : Model -> Cmd Msg
retrieveMysteries model =
    let
        url =
            model.config.baseURL ++ endpointMysteries ++ queryMysteries

        request =
            Http.get url decodeMysteries
    in
        Http.send NewMysteries request



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "CarCrashMystery" ]
        , button [ onClick RetrieveMysteries ] [ text "Retrieve mysteries" ]
        , ul [] 
            [ li [] [a [ href Translation.pathMystery ] [ text Translation.pathMystery ]]
            , li [] [a [ href Translation.pathDriver ] [ text Translation.pathDriver ]]
            , li [] [a [ href Translation.pathSmartphone ] [ text Translation.pathSmartphone ]]
            , li [] [a [ href Translation.pathContacts ] [ text Translation.pathContacts ]]
            , li [] [a [ href Translation.pathApps ] [ text Translation.pathApps ]]
            , li [] [a [ href Translation.pathMessages ] [ text Translation.pathMessages ]]
            , li [] [a [ href Translation.pathToDoList ] [ text Translation.pathToDoList ]]
            , li [] [a [ href Translation.pathBrowserHistory ] [ text Translation.pathBrowserHistory ]]
            , li [] [a [ href Translation.pathCallHistory ] [ text Translation.pathCallHistory ]]
            , li [] [a [ href Translation.pathWallet ] [ text Translation.pathWallet ]]
            , li [] [a [ href Translation.pathCreditCards ] [ text Translation.pathCreditCards ]]
            , li [] [a [ href Translation.pathKeys ] [ text Translation.pathKeys ]]
            , li [] [a [ href Translation.pathCar ] [ text Translation.pathCar ]]
            , li [] [a [ href Translation.pathGloveCompartment ] [ text Translation.pathGloveCompartment ]]
            , li [] [a [ href Translation.pathTrunk ] [ text Translation.pathTrunk ]]
            , li [] [a [ href Translation.pathCupHolder ] [ text Translation.pathCupHolder ]]
            , li [] [a [ href Translation.pathDamage ] [ text Translation.pathDamage ]]
            , li [] [a [ href Translation.pathDiary ] [ text Translation.pathDiary ]]
            , li [] [a [ href Translation.pathTags ] [ text Translation.pathTags ]]
            ]
        , overlay model
        ]


overlay : Model -> Html Msg
overlay model =
    case model.route of
        RouteLanding ->
            div [] []

        RouteMystery ->
            div [] [ text Translation.locationMystery ]

        RouteDriver ->
            div [] [ text Translation.locationDriver ]

        RouteSmartphone ->
            div [] [ text Translation.locationSmartphone ]

        RouteContacts ->
            div [] [ text Translation.locationContacts ]

        RouteApps ->
            div [] [ text Translation.locationApps ]

        RouteMessages ->
            div [] [ text Translation.locationMessages ]

        RouteToDoList ->
            div [] [ text Translation.locationToDoList ]

        RouteBrowserHistory ->
            div [] [ text Translation.locationBrowserHistory ]

        RouteCallHistory ->
            div [] [ text Translation.locationCallHistory ]
            
        RouteWallet ->
            div [] [ text Translation.locationWallet ]

        RouteCreditCards ->
            div [] [ text Translation.locationCreditCards ]

        RouteKeys ->
            div [] [ text Translation.locationKeys ]

        RouteCar ->
            div [] [ text Translation.locationCar ]

        RouteGloveCompartment ->
            div [] [ text Translation.locationGloveCompartment ]

        RouteTrunk ->
            div [] [ text Translation.locationTrunk ]

        RouteCupHolder ->
            div [] [ text Translation.locationCupHolder ]

        RouteDamage ->
            div [] [ text Translation.locationDamage ]

        RouteDiary ->
            div [] [ text Translation.locationDiary ]

        RouteTags ->
            div [] [ text Translation.locationTags ]

        RouteNotFound ->
            div [] [ text "Not found" ]
