module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import Translation.App as Translation
import UrlParser as UP
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Decode.Pipeline as JDP
import Date exposing (Date)
import Time exposing (Time)
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (class, xlinkHref)
import Dict exposing (Dict)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config, route : Route, location : Location, togglesVisibility : Toggles, mysteries : List Mystery, mystery : Mystery }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias Toggles =
    Dict String Bool


type alias Mystery =
    { id : String, driver : Driver, car : Car, mysteriesSolveTags : List MysterySolveTag }


type alias Driver =
    { id : Int, mysteryId : String, name : String, birthDate : Date, gender : String, race : String, email : String, address : String, smartphone : Smartphone, wallet : Wallet, keyNames : List KeyName }


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

        model =
            { config = { version = flags.version, baseURL = flags.baseURL }, route = currentRoute, location = location, togglesVisibility = Dict.empty, mysteries = [], mystery = makeEmptyMystery }
    in
        ( model, retrieveMysteryLatest model )


makeEmptyMystery : Mystery
makeEmptyMystery =
    Mystery "" makeEmptyDriver makeEmptyCar [ makeEmptyMysterySolveTag ]


makeEmptyDriver : Driver
makeEmptyDriver =
    Driver 0 "" "" (Date.fromTime (Time.millisecond * 0)) "" "" "" "" makeEmptySmartphone makeEmptyWallet [ makeEmptyKeyName ]


makeEmptySmartphone : Smartphone
makeEmptySmartphone =
    Smartphone 0 0 [ makeEmptyContact ] [ makeEmptyApp ] [ makeEmptyMessage ] [ makeEmptyToDoItem ] [ makeEmptyBrowserHistoryEntry ] [ makeEmptyCallHistoryEntry ]


makeEmptyContact : Contact
makeEmptyContact =
    Contact 0 0 ""


makeEmptyApp : App
makeEmptyApp =
    App 0 0 ""


makeEmptyMessage : Message
makeEmptyMessage =
    Message 0 0 "" "" (Date.fromTime (Time.millisecond * 0)) "" False


makeEmptyToDoItem : ToDoItem
makeEmptyToDoItem =
    ToDoItem 0 0 "" False (Date.fromTime (Time.millisecond * 0))


makeEmptyBrowserHistoryEntry : BrowserHistoryEntry
makeEmptyBrowserHistoryEntry =
    BrowserHistoryEntry 0 0 "" (Date.fromTime (Time.millisecond * 0))


makeEmptyCallHistoryEntry : CallHistoryEntry
makeEmptyCallHistoryEntry =
    CallHistoryEntry 0 0 "" (Date.fromTime (Time.millisecond * 0)) False False False


makeEmptyWallet : Wallet
makeEmptyWallet =
    Wallet 0 0 "" 0 "" [ makeEmptyCreditCard ]


makeEmptyCreditCard : CreditCard
makeEmptyCreditCard =
    CreditCard 0 0 "" (Date.fromTime (Time.millisecond * 0)) "" ""


makeEmptyKeyName : KeyName
makeEmptyKeyName =
    KeyName 0 0 ""


makeEmptyCar : Car
makeEmptyCar =
    Car 0 "" "" "" "" "" "" [ makeEmptyGloveCompartmentItem ] [ makeEmptyTrunkContentItem ] [ makeEmptyCupHolderContentItem ] [ makeEmptyDamageEntry ] [ makeEmptyDiaryEntry ]


makeEmptyGloveCompartmentItem : GloveCompartmentItem
makeEmptyGloveCompartmentItem =
    GloveCompartmentItem 0 0 ""


makeEmptyTrunkContentItem : TrunkContentItem
makeEmptyTrunkContentItem =
    TrunkContentItem 0 0 ""


makeEmptyCupHolderContentItem : CupHolderContentItem
makeEmptyCupHolderContentItem =
    CupHolderContentItem 0 0 ""


makeEmptyDamageEntry : DamageEntry
makeEmptyDamageEntry =
    DamageEntry 0 0 ""


makeEmptyDiaryEntry : DiaryEntry
makeEmptyDiaryEntry =
    DiaryEntry 0 0 0 (Date.fromTime (Time.millisecond * 0)) "" ""


makeEmptyMysterySolveTag : MysterySolveTag
makeEmptyMysterySolveTag =
    MysterySolveTag "" 0 makeEmptySolveTag


makeEmptySolveTag : SolveTag
makeEmptySolveTag =
    SolveTag 0 ""


checkVisibility : Model -> String -> Bool
checkVisibility model key =
    case Dict.get key model.togglesVisibility of
        Just value ->
            value

        Nothing ->
            False



-- DATA


endpointMysteries : String
endpointMysteries =
    "/mysteries"


queryMysteries : String
queryMysteries =
    "?select=id,created_date_time,driver:drivers(*,smartphone:smartphones(*,contacts(*),apps(*),messages(*),todo_items(*),browser_history_entries(*),call_history_entries(*)),wallet:wallets(*,credit_cards(*)),key_names(*)),car:cars(*,glove_compartment_items(*),trunk_content_items(*),cupholder_content_items(*),damage_entries(*),diary_entries(*)),mysteries_solve_tags(*,solve_tag:solve_tags(*))"


queryMystery : String -> String
queryMystery id =
    "?id=eq." ++ id ++ "&select=id,created_date_time,driver:drivers(*,smartphone:smartphones(*,contacts(*),apps(*),messages(*),todo_items(*),browser_history_entries(*),call_history_entries(*)),wallet:wallets(*,credit_cards(*)),key_names(*)),car:cars(*,glove_compartment_items(*),trunk_content_items(*),cupholder_content_items(*),damage_entries(*),diary_entries(*)),mysteries_solve_tags(*,solve_tag:solve_tags(*))"


queryMysteryLatest : String
queryMysteryLatest =
    "?limit=1&order=created_date_time.desc&select=id,created_date_time,driver:drivers(*,smartphone:smartphones(*,contacts(*),apps(*),messages(*),todo_items(*),browser_history_entries(*),call_history_entries(*)),wallet:wallets(*,credit_cards(*)),key_names(*)),car:cars(*,glove_compartment_items(*),trunk_content_items(*),cupholder_content_items(*),damage_entries(*),diary_entries(*)),mysteries_solve_tags(*,solve_tag:solve_tags(*))"



-- DECODERS


decodeMystery : JD.Decoder Mystery
decodeMystery =
    JDP.decode Mystery
        |> JDP.required "id" JD.string
        |> JDP.required "driver" (JD.index 0 decodeDriver)
        |> JDP.required "car" (JD.index 0 decodeCar)
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
        |> JDP.required "smartphone" (JD.index 0 decodeSmartphone)
        |> JDP.required "wallet" (JD.index 0 decodeWallet)
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
        |> JDP.required "todo_items" decodeToDoItems
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
        |> JDP.required "cupholder_content_items" decodeCupHolderContentItems
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
    | ToggleVisibility String
    | RetrieveMysteries
    | NewMysteries (Result Http.Error (List Mystery))
    | RetrieveMystery String
    | NewMystery (Result Http.Error Mystery)


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
                ( { model | route = newRoute, location = location }, Cmd.none )

        ToggleVisibility key ->
            let
                oldBool =
                    checkVisibility model key
            in
                ( { model | togglesVisibility = Dict.insert key (not oldBool) model.togglesVisibility }, Cmd.none )

        RetrieveMysteries ->
            ( model, retrieveMysteries model )

        NewMysteries (Ok newMysteries) ->
            ( { model | mysteries = newMysteries }, Cmd.none )

        NewMysteries (Err _) ->
            ( model, Cmd.none )

        RetrieveMystery id ->
            ( model, retrieveMystery model id )

        NewMystery (Ok newMystery) ->
            ( { model | mystery = newMystery }, Cmd.none )

        NewMystery (Err _) ->
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


retrieveMystery : Model -> String -> Cmd Msg
retrieveMystery model id =
    let
        url =
            model.config.baseURL ++ endpointMysteries ++ (queryMystery id)

        request =
            Http.get url (JD.index 0 decodeMystery)
    in
        Http.send NewMystery request


retrieveMysteryLatest : Model -> Cmd Msg
retrieveMysteryLatest model =
    let
        url =
            model.config.baseURL ++ endpointMysteries ++ queryMysteryLatest

        request =
            Http.get url (JD.index 0 decodeMystery)
    in
        Http.send NewMystery request



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "app-container", True ) ] ]
        [ viewMystery model
        , viewOverlay model
        ]


viewIcon : String -> String -> Html Msg
viewIcon name modifier =
    svg [ SA.class ("icon " ++ modifier) ]
        [ use [ xlinkHref ("icons.svg#" ++ name) ] [] ]


viewMystery : Model -> Html Msg
viewMystery model =
    div [ classList [ ( "mystery", True ) ] ]
        [ viewBuilding model
        , viewDriver model
        , viewCar model
        , viewTags model
        ]


viewBuilding : Model -> Html Msg
viewBuilding model =
    div [ classList [ ( "building", True ) ] ] []


viewDriver : Model -> Html Msg
viewDriver model =
    div [ classList [ ( "driver", True ) ] ]
        [ viewIcon "smartphone" ""
        , viewSmartphone model
        , viewIcon "wallet" ""
        , viewWallet model
        , viewIcon "key" ""
        , viewKeys model
        ]


viewSmartphone : Model -> Html Msg
viewSmartphone model =
    div [ classList [ ( "smartphone", True ) ] ]
        [ viewContacts model
        , viewApps model
        , viewMessages model
        , viewToDoList model
        , viewBrowserHistory model
        , viewCallHistory model
        ]


viewContacts : Model -> Html Msg
viewContacts model =
    div [ classList [ ( "smartphone-contacts", True ) ] ]
        []


viewApps : Model -> Html Msg
viewApps model =
    div [ classList [ ( "smartphone-apps", True ) ] ]
        []


viewMessages : Model -> Html Msg
viewMessages model =
    div [ classList [ ( "smartphone-messages", True ) ] ]
        []


viewToDoList : Model -> Html Msg
viewToDoList model =
    div [ classList [ ( "smartphone-to-do-list", True ) ] ]
        []


viewBrowserHistory : Model -> Html Msg
viewBrowserHistory model =
    div [ classList [ ( "smartphone-browser-history", True ) ] ]
        []


viewCallHistory : Model -> Html Msg
viewCallHistory model =
    div [ classList [ ( "smartphone-call-history", True ) ] ]
        []


viewWallet : Model -> Html Msg
viewWallet model =
    div [ classList [ ( "wallet", True ) ] ]
        [ viewCreditCards model ]


viewCreditCards : Model -> Html Msg
viewCreditCards model =
    div [ classList [ ( "wallet-credit-cards", True ) ] ]
        []


viewKeys : Model -> Html Msg
viewKeys model =
    div [ classList [ ( "keys", True ) ] ]
        []


viewCar : Model -> Html Msg
viewCar model =
    div [ classList [ ( "car", True ) ], onClick (ToggleVisibility "car-detail") ]
        [ viewCarDetail model
        ]


viewCarDetail : Model -> Html Msg
viewCarDetail model =
    div [ classList [ ( "car-detail", True ), ( "visible", (checkVisibility model "car-detail") ) ] ]
        [ viewIcon "hands" ""
        , viewGloveCompartment model
        , viewIcon "trunk" ""
        , viewTrunk model
        , viewIcon "drink" ""
        , viewCupHolder model
        , viewIcon "damage" ""
        , viewDamage model
        , viewIcon "book" ""
        , viewDiary model
        ]


viewGloveCompartment : Model -> Html Msg
viewGloveCompartment model =
    div [ classList [ ( "car-glove-compartment", True ) ] ]
        []


viewTrunk : Model -> Html Msg
viewTrunk model =
    div [ classList [ ( "car-trunk", True ) ] ]
        []


viewCupHolder : Model -> Html Msg
viewCupHolder model =
    div [ classList [ ( "car-cup-holder", True ) ] ]
        []


viewDamage : Model -> Html Msg
viewDamage model =
    div [ classList [ ( "car-damage", True ) ] ]
        []


viewDiary : Model -> Html Msg
viewDiary model =
    div [ classList [ ( "car-diary", True ) ] ]
        []


viewTags : Model -> Html Msg
viewTags model =
    div [ classList [ ( "tags", True ) ] ]
        []


viewCard : Model -> Html Msg
viewCard model =
    div [ classList [ ( "card", True ) ] ]
        []


viewOverlay : Model -> Html Msg
viewOverlay model =
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
