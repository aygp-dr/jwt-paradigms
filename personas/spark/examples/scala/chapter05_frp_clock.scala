val clock = Signal.periodic(1.second).map(_ => new Date())
val displayTime = clock.map(time => time.toLocaleTimeString())
val view = displayTime.map(timeStr => div("Current time: " + timeStr))
