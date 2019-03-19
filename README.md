# manufacturing_system_optimization
Linear Programming optimization for plant, product, supplier system

README

Author: Tejas Bala, Data Science Co-op GAMMA S&S
Date Created: 9/20/2017
Last Modified: 10/18/2017

Linear Optimization Solution

This solution, programmed in R, minimizes cost for a system while satisfying capacity and demand requirements.

List of files in Optimization Package:
1. lp_opt_final.R -- Optimization solution
2. OptimizationInput.xlsx
	PriceMatrix.csv -- Template price data input for solution
	DemandByPlant.csv -- Template demand data input for solution
	CapacityBySupplier.csv -- Template capacity data input for solution
3. OptimizationOutput.xlsx
4. OptimizationVignette.ppt -- Vignette detailing context and outcome of solution



The data necessary are price matrix, demand matrix, and supplier capacity.

Price Matrix: 
	Column 1: Supplier
	Column 2: Product
	Column 3-end: prices
	Column headers: Plants
	
Capacity:
	Column 1: Supplier
	Column 2: Max volume for supplier
	
Demand:
	Column 1: Plant
	Column 2: Product
	Column 3: Demand for that product at that plant
	
There is a matrix labeled oldVolume that is used for ordering and format, it is the same format as price so price can be used for this as well.

Throughout the code the logic is detailed and areas that require user input is marked with '@'.

Keep all files in the same folder. This will be your directory that is referenced in the code.

Steps:
1. Load data 
2. Order/Sort data to be in same order
3. Create dense matrix for only the variables that have non-zero prices and which constraint apply to them
	3a. Find all non-zero prices and the corresponding product/plant/supplier
	3b. Create empty matrix with constraints as row names and non-zero product/plant/supplier as column names
	3c. Fill matrix
4. Create model
	4a. Define variables : all non-zero product/plant/supplier
	4b. Define min or max
	4c. Define objective function: list of non-zero prices
	4d. Add constraints
5. Check constraint satisfaction
6. Write output files


How to run:
1. Open lp_opt_final.R
2. Review all areas marked with '@' to see if edits need to be made based on your specific data
3. Select all & run
4. View output files in working directory location
