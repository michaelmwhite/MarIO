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
    for i=1,NumPopulations do
        newNetwork = mutate(newNetwork())
        addToSpecies(newNetwork)
    end
    return pool
end

function newSpecies()
    local species = {}
    species.staleness = 0
    species.networks = {}
    return species
end

-- TODO: CHANGE IMPLEMENTATION - keep table of nodes, ensure when adding connections that it is proper direction (nodeNum - 
--  node never outputs to node with num less than it), then evaluate
-- or maybe not have list of nodes - somewhat pointless as nodes are just inputs (already stored) and outputs(already stored) w/ sigmoid function
function newNetwork()
    local network = {}
    network.connections = {}
    -- first x ids are reserved for inputs, next y ids are reserved for outputs, anything after is hidden layer node
    network.nodeCount = NumInputs + NumOutputs
    network.fitness = 0
    network.rank = 0
    return network
end

function copyNetwork(genome)
	-- TODO
end

function basicNetwork()
	-- TODO
end

function newConnection(network)
    local connection = {}
    connection.inputId = 0
    connection.outputId = 0
    connection.weight = (math.random()-.5) * 2
    connection.enabled = true
    connection.innovationNum = nextInnovationNum(network)
    return connection
end

function copyConnection(connection)
    local copyConnection = {}
    copyConnection.inputId = connection.inputId
    copyConnection.outputId = connection.outputId
    copyConnection.weight = connection.weight
    copyConnection.enabled = connection.enabled
    copyConnection.innovationNum = connection.innovationNum
    return copyConnection
end

function newNode()
    local node = {}
    node.value = 0.0
    node.inputConnections = {}
    return node
end

-- rather than have to keep track of nodes and combinations, generate nodes in network at time of evaluation
function buildNodes(network)
    local nodes = {}
    -- generate input and output nodes - this part is not chronologically ordered
    for i=1,NumInputs+NumOutputs do
        nodes[i] = newNode()
    end
    -- NOTE: important not to ignore disabled when building node network - this should be entirely deterministic as index numbers represent
    -- id numbers in my system
    for i=1,#network.connections do 
        local connection = network.connections[i]
        -- make all appropriate nodes
        if nodes[connection.inputId] == nil then
            nodes[connection.inputId] = newNode()
        end
        if nodes[connection.outputId] == nil then
            nodes[connection.outputId] = newNode()
        end
        table.insert(nodes[connection.outputId].inputConnections, connection)
    end
    return nodes
end

function evaluateNetwork(network)
    local outputCommands = {}

    local nodes = buildNodes(network)
    local inputs = getInputs()
    if #inputs ~= NumInputs then
		print("Incorrect number of neural network inputs.")
		return
    end
    -- fill in inputs
    for i=1,NumInputs do
        nodes[i].value = inputs[i]
    end
    -- calculate hidden layer nodes
    for i=NumInputs+NumOutputs+1,#nodes do
        local sum = 0
        for j=1,#nodes[i].inputConnections do
            local connection = nodes[i].inputConnections[j]
            if connection.enabled then
                sum = sum + connection.weight * nodes[connection.inputId].value
            end
        end
        nodes[i].value = sigmoid(sum)
    end
    -- calculate output nodes
    for i=NumInputs+1,NumOutputs do
        local sum = 0
        for j=1,#nodes[i].inputConnections do
            local connection = nodes[i].inputConnections[j]
            if connection.enabled then
                sum = sum + connection.weight * nodes[connection.inputId].value
            end
        end
        nodes[i].value = sigmoid(sum)
        -- write command for controller
        local button = "P1 " .. ButtonNames[i-NumInputs+1]
        if nodes[i].value > 0 then
            outputCommands[button] = true
        else
            outputCommands[button] = false
        end
    end
	return outputCommands
end

function crossover(g1, g2)
	-- TODO
end

--return if connection exists, regardless of weights
function containsConnection(genes, link)
	-- TODO
end

-- if mutating connections, change gene weights in genome
function connectionWeightsMutate(network)
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
function addConnectionMutate(network)
    if network.nodeCount < 2 then
        print("nodeCount should always be greater than 1")
        return
    end
    local node1Id = math.random(network.nodeCount)
    local node2Id = math.random(network.nodeCount)
    while node1Id == node2Id do
        node2Id = math.random(network.nodeCount)
    end
    local newConnection = newConnection()
    -- NOTE: CYCLICAL CONNECTIONS A PROBLEM - address in future, for now, will have value of 0.0 at time of evaluation
    newConnection.inputId = node1Id
    newConnection.outputId = node2Id
    table.insert(network.connections, newConnection)
end

-- mutate a single connection to become two connections with a node inbetween that is mathematically identical for the current run
function addNodeMutate(network)
    if #network.connections == 0 then
        return
    end
    network.nodeCount = network.nodeCount + 1
    local connection = network.connections[math.random(#network.connections)]

    local connection1 = copyConnection()
    connection1.outputId = network.nodeCount
    connection1.weight = 1.0
    connection1.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection1)

    local connection2 = copyConnection()
    connection2.inputId = network.nodeCount
    connection2.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection2)
end

-- mutate to either enable or disable a connection
function enableMutate(network)
    local disabledConnections = {}
    for i=1,#network.connections do
        if not network.connections[i].enabled then
            table.insert(disabledConnections, network.connections[i])
        end
    end
    if #disabledConnections == 0 then
        return
    end
    disabledConnections[math.random(#disabledConnections)].enabled = true
end

function disableMutate(network)
	local enabledConnections = {}
    for i=1,#network.connections do
        if network.connections[i].enabled then
            table.insert(enabledConnections, network.connections[i])
        end
    end
    if #enabledConnections == 0 then
        return
    end
    enabledConnections[math.random(#enabledConnections)].enabled = true
end

function mutate(network)
    if math.random() < MutateConnectionsChance then
        connectionWeightsMutate(network)
    end
    if math.random() < LinkMutationChance then
        addConnectionMutate(network)
    end
    if math.random() < NodeMutationChance then
        addNodeMutate(network)
    end
    if math.random() < EnableMutationChance then
        enableMutate(network)
    end
    if math.random() < DisableMutationChance then
        disableMutate(network)
    end
end

-- return the number of disjoint genes between two sets of genes
function numDisjointOrExcess(connections1, connections2)
    local disjointExcessCount = 0
    local innovations1 = {}
    for i=1,#connections1 do
        if connections1[i].enabled then 
            innovations1[connections1[i].innovationNum] = true
        end
    end
    local innovations2 = {}
    for i=1,#connections2 do
        if connections2[i].enabled then
            innovations2[connections2[i].innovationNum] = true
        end
    end
    for i=1,#connections1 do
        if not innovations2[connections1[i].innovationNum] then
            disjointExcessCount = disjointExcessCount + 1
        end
    end
    for i=1,#connection2 do
        if not innovations1[connections2[i].innovationNum] then
            disjointExcessCount = disjointExcessCount + 1
        end
    end
    return disjointExcessCount
end

-- calculates average difference in weights per genes in genomes
function avgWeightsDiff(connections1, connections2)
    local sum = 0
    local numDiff = 0
    local innovations2 = {}
    for i=1,#connections2 do
        innovations2[connections2[i].innovationNum] = connections2[i]
    end
    for i=1,#connections1 then
        local connection2 = innovations2[connections1[i].innovationNum]
        if connection2 ~= nil then
            sum = sum + math.abs(connections1[i].weight - connection2.weight)
            numDiff = numDiff + 1
        end
    end
    return sum / numDiff
end
	
-- determin if same species - roughly equivalent of equation 1 in Stanley's paper
function isSameSpecies(network1, network2)
    local disjointDelta = DeltaDisjoint * numDisjointOrExcess(network1.connections, network2.connections)
    local weightDelta = DeltaWeights * avgWeightsDiff(network1.connections, nertwork2.connections)
    return disjointDelta + weightDelta < DeltaThreshold
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
function addToSpecies(network)
    for i=1,#pool.species do
        if isSameSpecies(pool.species[i].networks[1], network) then
            table.insert(pool.species[i].networks, network)
            return
        end
    end
    local newSpecies = newSpecies()
    table.insert(newSpecies.networks, network)
    table.insert(pool.species, newSpecies)
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

-- pick next genome or loop back around and make the next generation
function nextNetwork()
	-- TODO
end

function fitnessAlreadyMeasured()
	-- TODO
end

-- TODO: END OF NN CODE
-- BEGINNING OF "MAIN()"

--INITIALIZE RUN CODE - TODO - REWORK!
pool = newPool()
savestate.load(Filename)
rightmost = 0
timeout = TimeoutConstant
clearJoypad()
local species = pool.species[pool.currentSpecies]
local genome = species.genomes[pool.currentGenome]
generateNetwork(genome)
evaluateCurrent()


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