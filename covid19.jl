try using JuliaInXL
catch
    Pkg.add("JuliaInXL")
    Pkg.build("JuliaInXL")
    using JuliaInXL
end

# from https://github.com/queryverse/ExcelFiles.jl
try using ExcelFiles
catch
    Pkg.add("ExcelFiles")
    using ExcelFiles
end

try using DataFrames
catch
    Pkg.add("DataFrames")
    using DataFrames
end
df = DataFrame(load("data.xlsx", "Sheet1"))
using ExcelFiles, DataTables, IndexedTables, TimeSeries, Temporal, Gadfly

# Load into a DataTable
dt = DataTable(load("data.xlsx", "Sheet1"))

# Load into an IndexedTable
it = IndexedTable(load("data.xlsx", "Sheet1"))

# Load into a TimeArray
ta = TimeArray(load("data.xlsx", "Sheet1"))

# Load into a TS
ts = TS(load("data.xlsx", "Sheet1"))

# Plot directly with Gadfly
plot(load("data.xlsx", "Sheet1"), x=:a, y=:b, Geom.line)

function load(f::FileIO.File{FileIO.format"Excel"}, range; keywords...)
#Arguments:
#range: either the name of the sheet in the Excel file to read, or a full Excel range specification (i.e. "Sheetname!A1:B2").
#The keywords arguments are the same as in ExcelReaders.jl (which is used under the hood to read Excel files). When range is a sheet name, the keyword arguments for the readxlsheet function from ExcelReaders.jl apply, if range is a range specification, the keyword arguments for the readxl function apply.
#Save an Excel file
#The following code saves any iterable table as an excel file:

using ExcelFiles

save("output.xlsx", it)
#This will work as long as it is any of the types supported as sources in IterableTables.jl.

#Using the pipe syntax
#load also support the pipe syntax. For example, to load an Excel file into a DataFrame, one can use the following code:

using ExcelFiles, DataFrame

df = load("data.xlsx", "Sheet1") |> DataFrame
#To save an iterable table, one can use the following form:

using ExcelFiles, DataFrame

df = # Aquire a DataFrame somehow

df |> save("output.xlsx")
#The pipe syntax is especially useful when combining it with Query.jl queries, for example one can easily load an Excel file, pipe it into a query, then pipe it to the save function to store the results in a new file.
