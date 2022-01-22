namespace TicTacToe.ViewModels

open ReactiveUI

open LibTicTacToe.Domain
open LibTicTacToe.Logic
open TicTacToe.Models

type BotOptionsWindowViewModel () as this =
    inherit ViewModelBase ()

    let unlimitedIndex = 0
    let depthIndex     = 1
    let timeIndex      = 2

    let mutable xLimiterType = unlimitedIndex
    let mutable xMaxDepth = 10
    let mutable xTimeLimitSeconds = 10

    let mutable oLimiterType = unlimitedIndex
    let mutable oMaxDepth = 10
    let mutable oTimeLimitSeconds = 10

    let isValidLimiterType index =
        index = unlimitedIndex ||
        index = depthIndex ||
        index = timeIndex

    let isConfigurationValid () =
        isValidLimiterType xLimiterType &&
        xMaxDepth > 0 &&
        xTimeLimitSeconds > 0 &&

        isValidLimiterType oLimiterType &&
        oMaxDepth > 0 &&
        oTimeLimitSeconds > 0

    let getLimiter limiterType depth maxSeconds =
        if limiterType = unlimitedIndex then limitUnlimited
        elif limiterType = depthIndex then limitByDepth depth
        elif limiterType = timeIndex then limitByTime (System.TimeSpan.FromSeconds maxSeconds)
        else failwith "Invalid limiter type value."

    let limiterToTripletString limiter =
        match limiter with
        | Unlimited -> (unlimitedIndex, 10, 10)
        | Depth (_, max) -> (depthIndex, max, 10)
        | Time (max, _) -> (timeIndex, 10, max.TotalSeconds |> int)

    member _.XLimiterType
        with get () = xLimiterType
        and set value =
            this.RaiseAndSetIfChanged(&xLimiterType, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)
            this.RaisePropertyChanged (nameof this.IsXDepthLimit)
            this.RaisePropertyChanged (nameof this.IsXTimeLimit)

    member _.IsXDepthLimit
        with get () = this.XLimiterType = depthIndex

    member _.IsXTimeLimit
        with get () = this.XLimiterType = timeIndex

    member _.XMaxDepth
        with get () = xMaxDepth
        and set value =
            this.RaiseAndSetIfChanged(&xMaxDepth, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)

    member _.XTimeLimitSeconds
        with get () = xTimeLimitSeconds
        and set value =
            this.RaiseAndSetIfChanged(&xTimeLimitSeconds, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)

    member _.OLimiterType
        with get () = oLimiterType
        and set value =
            this.RaiseAndSetIfChanged(&oLimiterType, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)
            this.RaisePropertyChanged (nameof this.IsODepthLimit)
            this.RaisePropertyChanged (nameof this.IsOTimeLimit)

    member _.IsODepthLimit
        with get () = this.OLimiterType = depthIndex

    member _.IsOTimeLimit
        with get () = this.OLimiterType = timeIndex

    member _.OMaxDepth
        with get () = oMaxDepth
        and set value =
            this.RaiseAndSetIfChanged(&oMaxDepth, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)

    member _.OTimeLimitSeconds
        with get () = oTimeLimitSeconds
        and set value =
            this.RaiseAndSetIfChanged(&oTimeLimitSeconds, value) |> ignore
            this.RaisePropertyChanged (nameof this.IsConfigurationValid)

    member _.IsConfigurationValid = isConfigurationValid ()

    member _.GetBotOptionsModel () =
        match isConfigurationValid () with
        | false -> failwith "Invalid configuration values."
        | true -> { OLimit = getLimiter oLimiterType oMaxDepth oTimeLimitSeconds
                    XLimit = getLimiter xLimiterType xMaxDepth xTimeLimitSeconds }

    member _.PopulateFromBotOptionsModel (model: BotOptionsModel) =
        let xLimit, xDepth, xSeconds = limiterToTripletString model.XLimit
        let oLimit, oDepth, oSeconds = limiterToTripletString model.OLimit

        this.XLimiterType <- xLimit
        this.XMaxDepth <- xDepth
        this.XTimeLimitSeconds <- xSeconds

        this.OLimiterType <- oLimit
        this.OMaxDepth <- oDepth
        this.OTimeLimitSeconds <- oSeconds



