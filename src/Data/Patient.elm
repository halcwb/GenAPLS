module EmergencyList.Patient
    exposing
        ( Patient
        , getAge
        , getWeight
        , calculate
        , toString
        , Dto
        , dto
        , dtoToPatient
        , patientToDto
        )

import Time.Date as Date exposing (Date, date, delta, toISO8601, fromISO8601)
import FormatNumber exposing (format)
import Util.Locals exposing (locale1)


min_age =
    0


max_age =
    18


min_weight =
    0.3


max_weight =
    150


type Patient
    = NoPatient
    | Age Float
    | Weight Float
    | AgeAndWeight ( Float, Float )
    | BirthDate Date
    | BirthDateAndWeight ( Date, Float )


noPatient =
    NoPatient


checkMinMax : Float -> Float -> Float -> Maybe Float
checkMinMax min max n =
    if n >= min && n <= max then
        Just n
    else
        Nothing


checkAge : Float -> Maybe Float
checkAge =
    checkMinMax min_age max_age


checkWeight : Float -> Maybe Float
checkWeight =
    checkMinMax min_weight max_weight


checkAgeAndWeight : ( Float, Float ) -> Maybe ( Float, Float )
checkAgeAndWeight ( a, w ) =
    case ( a |> checkAge, w |> checkWeight ) of
        ( Just _, Just _ ) ->
            ( a, w ) |> Just

        _ ->
            Nothing


checkBirthDate : Date -> Date -> Maybe Date
checkBirthDate dt bd =
    let
        age =
            (delta dt bd).years |> toFloat
    in
        case checkAge age of
            Just _ ->
                Just bd

            Nothing ->
                Nothing


checkBirthDateAndWeight : Date -> ( Date, Float ) -> Maybe ( Date, Float )
checkBirthDateAndWeight dt ( bd, w ) =
    case ( checkBirthDate dt bd, w |> checkWeight ) of
        ( Just _, Just _ ) ->
            ( bd, w ) |> Just

        _ ->
            Nothing


checkPatient : (a -> Patient) -> (a -> Maybe a) -> a -> Patient
checkPatient p c a =
    case a |> c of
        Just a ->
            a |> p

        Nothing ->
            NoPatient


patientAge : Float -> Patient
patientAge =
    checkPatient Age checkAge


patientWeight : Float -> Patient
patientWeight =
    checkPatient Weight checkWeight


patientAgeAndWeight : ( Float, Float ) -> Patient
patientAgeAndWeight =
    checkPatient AgeAndWeight checkAgeAndWeight


patientBirthDate : String -> String -> Patient
patientBirthDate dt bd =
    case ( dt |> fromISO8601, bd |> fromISO8601 ) of
        ( Ok dt_, Ok bd_ ) ->
            checkPatient BirthDate (checkBirthDate dt_) bd_

        _ ->
            NoPatient


patientBirthDateAndWeight : String -> String -> Float -> Patient
patientBirthDateAndWeight dt bd w =
    case ( dt |> fromISO8601, bd |> fromISO8601 ) of
        ( Ok dt_, Ok bd_ ) ->
            checkPatient BirthDateAndWeight (checkBirthDateAndWeight dt_) ( bd_, w )

        _ ->
            NoPatient


getAge : Date -> Patient -> Maybe Float
getAge dt p =
    case p of
        Age a ->
            Just a

        AgeAndWeight ( a, _ ) ->
            Just a

        BirthDate bd ->
            calcAge dt bd |> Just

        BirthDateAndWeight ( bd, _ ) ->
            calcAge dt bd |> Just

        _ ->
            Nothing


getWeight : Patient -> Maybe Float
getWeight p =
    case p of
        Weight w ->
            Just w

        AgeAndWeight ( _, w ) ->
            Just w

        BirthDateAndWeight ( _, w ) ->
            Just w

        _ ->
            Nothing


calcWeight : Float -> Float
calcWeight a =
    let
        age_zero_weight =
            3.5

        age_6mo_weight =
            6
    in
        if a == 0 then
            age_zero_weight
        else if a > 0 && a <= 0.5 then
            age_6mo_weight
        else
            a * 2.5 + 8


calcAge : Date -> Date -> Float
calcAge dt bd =
    (delta dt bd).years |> toFloat


calculate : Date -> Patient -> Patient
calculate dt p =
    case p of
        Age a ->
            ( a, calcWeight a ) |> AgeAndWeight

        BirthDate bd ->
            ( bd, calcWeight ((delta dt bd).years |> toFloat) )
                |> BirthDateAndWeight

        _ ->
            p


type alias Dto =
    { age : Float
    , weight : Float
    , date : String
    , birthDate : String
    }


dto =
    { age = -1, weight = 0, date = "", birthDate = "" }


dtoToPatient : Dto -> Patient
dtoToPatient dto =
    let
        age =
            dto.age |> patientAge

        weight =
            dto.weight |> patientWeight

        ageAndWeight =
            patientAgeAndWeight ( dto.age, dto.weight )

        birthDate =
            patientBirthDate dto.date dto.birthDate

        birthDateAndWeight =
            patientBirthDateAndWeight dto.date dto.birthDate dto.weight
    in
        case ( birthDateAndWeight, ageAndWeight, birthDate, age ) of
            ( BirthDateAndWeight _, _, _, _ ) ->
                birthDateAndWeight

            ( _, AgeAndWeight _, _, _ ) ->
                ageAndWeight

            ( _, _, BirthDate _, _ ) ->
                birthDate

            ( _, _, _, Age _ ) ->
                age

            _ ->
                NoPatient


patientToDto : Date -> Patient -> Dto
patientToDto dt p =
    case p of
        NoPatient ->
            dto

        Age a ->
            { dto | age = a }

        Weight w ->
            { dto | weight = w }

        AgeAndWeight ( a, w ) ->
            { dto | age = a, weight = w }

        BirthDate bd ->
            { dto
                | age = calcAge dt bd
                , date = dt |> Date.toISO8601
                , birthDate = bd |> Date.toISO8601
            }

        BirthDateAndWeight ( bd, w ) ->
            { dto
                | age = calcAge dt bd
                , date = dt |> Date.toISO8601
                , birthDate = bd |> Date.toISO8601
                , weight = w
            }


toString : Date -> Patient -> String
toString dt p =
    let
        ageToString a =
            "Leefijd: " ++ format locale1 a ++ " jaar"

        weightToString w =
            "Gewicht: " ++ format locale1 w ++ " kg"

        birthDateToString bd =
            "Geboortedatum: " ++ (bd |> toISO8601)
    in
        case p of
            NoPatient ->
                ""

            Age a ->
                ageToString a

            Weight w ->
                weightToString w

            AgeAndWeight ( a, w ) ->
                ageToString a
                    ++ ", "
                    ++ (weightToString w)

            BirthDate bd ->
                (bd |> birthDateToString)
                    ++ ", "
                    ++ (calcAge dt bd |> ageToString)

            BirthDateAndWeight ( bd, w ) ->
                (bd |> birthDateToString)
                    ++ ", "
                    ++ (calcAge dt bd |> ageToString)
                    ++ ", "
                    ++ (weightToString w)
