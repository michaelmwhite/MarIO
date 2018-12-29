-- My own MarI/O implementation.
-- Inspired by SethBling and some of this code pulls from his implementation.
-- Works with the BizHawk emulator and Super Mario World (USA) ROM.
-- Have a save "DP1.state" set to the beginning of a level, saved where this script is.
-- Load the script in BizHawk's Lua Console when Super Mario is loaded.

print(gameinfo.getromname())

Filename = "DP1.state"
ButtonNames = {
    "A",
    "B",
    "X",
    "Y",
    "Up",
    "Down",
    "Left",
    "Right",
}

BoxRadius = 6

NumInputs = (BoxRadius*2+1)*(BoxRadius*2+1)+1
NumOutputs = #ButtonNames

NumPopulations = 300
DeltaDisjoint = 2.0
DeltaWeights = 0.4
DeltaThreshold = 1.0

StaleSpecies = 15

MutateConnectionsChance = 0.25
PerturbChance = 0.90
CrossoverChance = 0.75
LinkMutationChance = 2.0
NodeMutationChance = 0.50
StepSize = 0.1
DisableMutationChance = 0.4
EnableMutationChance = 0.2

TimeoutConstant = 20

MaxNodes = 1000000


-- code created by SethBling
function getPositions()
	marioX = memory.read_s16_le(0x94)
    marioY = memory.read_s16_le(0x96)
    
    local layer1x = memory.read_s16_le(0x1A);
    local layer1y = memory.read_s16_le(0x1C);
    
    screenX = marioX-layer1x
    screenY = marioY-layer1y
end

function getTile(dx, dy)
	x = math.floor((marioX+dx+8)/16)
    y = math.floor((marioY+dy)/16)
    -- blocks are contiguously in memory by row - go to offset x row (rows are 27 blocks wide, mario's x can be anywhere between 2 blocks)
    return memory.readbyte(0x1C800 + math.floor(x/0x10)*0x1B0 + y*0x10 + x%0x10)
end

function getSprites()
    local sprites = {}
    for slot=0,11 do
        local status = memory.readbyte(0x14C8+slot)
        if status ~= 0 then
            -- get low byte and then add high byte
            spritex = memory.readbyte(0xE4+slot) + memory.readbyte(0x14E0+slot)*256
            spritey = memory.readbyte(0xD8+slot) + memory.readbyte(0x14D4+slot)*256
            sprites[#sprites+1] = {["x"]=spritex, ["y"]=spritey}
        end
    end		
    
    return sprites
end

function getExtendedSprites()
	local extended = {}
    for slot=0,11 do
        local number = memory.readbyte(0x170B+slot)
        if number ~= 0 then
            spritex = memory.readbyte(0x171F+slot) + memory.readbyte(0x1733+slot)*256
            spritey = memory.readbyte(0x1715+slot) + memory.readbyte(0x1729+slot)*256
            extended[#extended+1] = {["x"]=spritex, ["y"]=spritey}
        end
    end		
    
    return extended
end

function getInputs()
	getPositions()
	
	sprites = getSprites()
	extended = getExtendedSprites()
	
	local inputs = {}
	
	-- "radius" - square minimap surrounding player that is used as inputs for the neural network
	for dy=-BoxRadius*16,BoxRadius*16,16 do
		for dx=-BoxRadius*16,BoxRadius*16,16 do
			inputs[#inputs+1] = 0
			
			tile = getTile(dx, dy)
			-- number of tiles per screen on horizontal levels 
			if tile == 1 and marioY+dy < 0x1B0 then
				inputs[#inputs] = 1
			end
			
			for i = 1,#sprites do
				distx = math.abs(sprites[i]["x"] - (marioX+dx))
				disty = math.abs(sprites[i]["y"] - (marioY+dy))
				if distx <= 8 and disty <= 8 then
					inputs[#inputs] = -1
				end
			end

			for i = 1,#extended do
				distx = math.abs(extended[i]["x"] - (marioX+dx))
				disty = math.abs(extended[i]["y"] - (marioY+dy))
				if distx < 8 and disty < 8 then
					inputs[#inputs] = -1
				end
			end
		end
	end
	
	--mariovx = memory.read_s8(0x7B)
	--mariovy = memory.read_s8(0x7D)
	
	return inputs
end

-- My original code, based on SethBling's implementation of a NEAT Network.
-- TODO: START OF NN CODE
function sigmoid(x)
	return 2/(1+math.exp(-4.9*x))-1
end

function nextInnovationNum(network)
    local currentInnovationNum = network.innovationNum
    network.innovationNum = network.innovationNum + 1
    return currentInnovationNum
end

-- arrays in Lua begin with 1
function newPool()
    local pool = {}
    pool.species = {}
    pool.generation = 0
    pool.innovationNum = NumOutputs
    pool.currentSpecies = 1
    pool.currentGenome = 1
    pool.currentFrame = 0
    pool.maxFitness = 0

    for i=1,NumPopulations do
        newNetwork = mutate(newNetwork())
        addToSpecies(newNetwork)
    end

    -- initializerun

    return pool
end

function newSpecies()
	-- TODO
end

function newNetwork()
    local network = {}
    network.connections = {}
    network.nodeCount = NumInputs
    network.fitness = 0
    network.rank = 0
end

function copyNetwork(genome)
	-- TODO
end

function basicNetwork()
	-- TODO
end

function newConnection(network)
    local connection = {}
    connection.input = 0
    connection.output = 0
    connection.weight = (math.random()-.5) * 2
    connection.enabled = true
    connection.innovationNum = nextInnovationNum(network)
    return connection
end

function copyConnection(connection)
    local copyConnection = {}
    copyConnection.input = connection.input
    copyConnection.output = connection.output
    copyConnection.weight = connection.weight
    copyConnection.enabled = connection.enabled
    copyConnection.innovationNum = connection.innovationNum
    return copyConnection
end

function newNode()
	-- TODO
end

function buildNetwork(genome)
	-- TODO
end

function evaluateNetwork(network, inputs)
	-- TODO
end

function crossover(g1, g2)
	-- TODO
end

--return if connection exists, regardless of weights
function containsConnection(genes, link)
	-- TODO
end

-- if mutating connections, change gene weights in genome
function pointMutate(network)
    for i=1,#network.connections do
        local connection = network.connections[i]
        if math.random() < PerturbChance then
            connection.weight = connection.weight + (math.random() - .5) * step
        else
            connection.weight = (math.random() - .5) * 2
        end
    end
end

-- add a new connection to the genome between two random nodes
function connectionMutate(network)
    local node1 = network.connections[math.random(#network.connections)]
    local node2 = network.connections[math.random(#network.connections)]
    if node1 == node2 then
        return
    end
    local newConnection = newConnection()
    -- TODO: CYCLICAL CONNECTIONS A PROBLEM? - make 1 always an input?
    newConnection.input = node1
    newConnection.output = node2
    table.insert(network.connections, newConnection)
end

-- mutate a single connection to become two connections with a node inbetween that is mathematically identical for the current run
function nodeMutate(network)
    if #network.connections == 0 then
        return
    end
    network.nodeCount = network.nodeCount + 1
    local connection = network.connections[math.random(#network.connections)]
    if not connection.enabled then
        return
    end
    local connection1 = copyConnection()
    connection1.output = network.nodeCount
    connection1.weight = 1.0
    connection1.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection1)

    local connection2 = copyConnection()
    connection2.input = network.nodeCount
    connection2.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection2)
end

-- mutate to either enable or disable a connection - which action to take is determined by parameter
function enableMutate(genome)
	-- TODO: HERE
end

function disableMutate(genome)
	-- TODO
end

function mutate(network)
    if math.random() < MutateConnectionsChance then
        pointMutate(network)
    end
    if math.random() < LinkMutationChance then
        connectionMutate(network, false)
    end
    if math.random() < NodeMutationChance then
        nodeMutate(network)
    end
    if math.random() < EnableMutationChance then
        enableMutate(network)
    end
    if math.random() < DisableMutationChance then
        disableMutate(network)
    end
end

-- return the number of disjoint genes between two sets of genes
function disjoint(genes1, genes2)
	-- TODO
end

-- calculates average difference in weights per genes in genomes
function weights(genes1, genes2)
	-- TODO
end
	
-- determin if same species - roughly equivalent of equation 1 in Stanley's paper
function sameSpecies(genome1, genome2)
	-- TODO
end

-- rank all genomes regardless of species
function rankGlobally()
	-- TODO
end

-- calculate average rank of a species
function calculateAverageFitness(species)
	-- TODO
end

-- total up the average fitnesses of each species
function totalAverageFitness()
	-- TODO
end

-- either halve species or cut to only 1 genome
function cullSpecies(cutToOne)
	-- TODO
end

-- crossover a child or simply copy 1 and then mutate
function breedChild(species)
	-- TODO
end

-- add only species to next generation that aren't stale
function removeStaleSpecies()
	-- TODO
end

-- only keep species with fitness in upper half
function removeWeakSpecies()
	-- TODO
end

-- sort passed in genome to appropriate species
function addToSpecies(child)
	-- TODO
end

function newGeneration()
	-- TODO
end
	
-- create initial pool
function initializePool()
	-- TODO
end

-- reset control inputs to all false
function clearJoypad()
	controller = {}
	for b = 1,#ButtonNames do
		controller["P1 " .. ButtonNames[b]] = false
	end
	joypad.set(controller)
end

function initializeRun()
	-- TODO
end

function evaluateCurrent()
    -- TODO
end

if pool == nil then
	initializePool()
end

-- pick next genome or loop back around and make the next generation
function nextNetwork()
	-- TODO
end

function fitnessAlreadyMeasured()
	-- TODO
end

-- TODO: END OF NN CODE

-- a lot of gui updating code, but also handles running neural network/ indirectly updating when network has finished its run
while true do
	-- evaluate network every 5 frames to see next control that should be done
	if pool.currentFrame%5 == 0 then
		-- get controls neural network says to use
		evaluateCurrent()
	end

	joypad.set(controller)

	-- if mario has not moved to right, begin frame count down until considered timed out
	getPositions()
	if marioX > rightmost then
		rightmost = marioX
		timeout = TimeoutConstant
	end
	
	timeout = timeout - 1
	
	-- give more lenience on timeout farther into the level you are
	local timeoutBonus = pool.currentFrame / 4
	if timeout + timeoutBonus <= 0 then
		local fitness = rightmost - pool.currentFrame / 2
		if rightmost > 4816 then
			fitness = fitness + 1000
		end
		if fitness == 0 then
			fitness = -1
		end
		-- fitness is the indicator of where the program is currently executing in this weird loop
		genome.fitness = fitness
		
		-- handle if new fitness record
		
		-- reset currentspecies and current genome and increment them until you find a non measure genome; once found, reset for new run

    end
    
	pool.currentFrame = pool.currentFrame + 1

	emu.frameadvance();
end