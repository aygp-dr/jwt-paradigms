// Functional data processing
let calculateStatistics data =
    let average = Seq.average data
    let stdDev = 
        data
        |> Seq.map (fun x -> (x - average) ** 2.0)
        |> Seq.average
        |> sqrt
    (average, stdDev)

// Object-oriented interface to database
type DataRepository(connectionString) =
    member this.GetData() =
        // Note: This is simplified for the example
        // In a real implementation, this would use actual database access
        printfn "Connecting to database with %s" connectionString
        [1.0; 2.0; 3.0; 4.0; 5.0]
        
    member this.SaveStatistics(average, stdDev) =
        // Simplified database operation
        printfn "Saving statistics to database: avg=%f, stdDev=%f" average stdDev

// Combining the approaches
let processData connectionString =
    let repository = DataRepository(connectionString)
    let data = repository.GetData()
    let stats = calculateStatistics data
    repository.SaveStatistics stats
    stats

// Test the function
let avg, std = processData "server=localhost;database=testdb"
printfn "Result: average=%f, stdDev=%f" avg std
