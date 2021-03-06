-- My own MarI/O implementation.
-- Inspired by SethBling and some of this code pulls from his implementation.
-- Works with the BizHawk emulator and Super Mario World (USA) ROM.
-- Have a save "DP1.state" set to the beginning of a level, saved where this script is.
-- Load the script in BizHawk's Lua Console when Super Mario is loaded.

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

NumPopulations = 100
DeltaDisjoint = 1.0
DeltaWeights = 0.2
DeltaThreshold = 5

StaleSpecies = 5

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
	return inputs
end

-- My original code, based on SethBling's implementation of a NEAT Network.
-- NOTE: START OF NN CODE
function sigmoid(x)
	return 2/(1+math.exp(-4.9*x))-1
end

function nextInnovationNum(network)
    local currentInnovationNum = network.innovationNum
    network.innovationNum = network.innovationNum + 1
    return currentInnovationNum
end

function newNetwork()
    local network = {}
    network.connections = {}
    -- first x ids are reserved for inputs, next y ids are reserved for outputs, anything after is hidden layer node
    network.nodeCount = NumInputs + NumOutputs
    network.fitness = 0
    network.innovationNum = 0
    return network
end

function initPool()
    pool = {}
    pool.species = {}
    pool.generation = 1
    pool.currentSpecies = 1
    pool.currentNetwork = 1
    pool.currentFrame = 0
    for i=1,NumPopulations do
        local newNetwork = mutate(newNetwork())
        addToSpecies(newNetwork)
    end
    print("Initialized pool with " .. #pool.species .. " species and " .. NumPopulations .. " networks")
end

function newSpecies()
    local species = {}
    species.staleness = 0
    species.prevMaxFitness = 0
    species.averageFitness = 0
    species.networks = {}
    return species
end

function copyNetwork(network)
    local newNetwork = newNetwork()
    for i=1,#network.connections do
        table.insert(newNetwork.connections, copyConnection(network.connections[i]))
    end
    newNetwork.nodeCount = network.nodeCount
    newNetwork.fitness = network.fitness
    newNetwork.innovationNum = network.innovationNum
    return newNetwork
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
    -- NOTE: important not to ignore disabled when building node network - this should be entirely deterministic as index numbers represent
    -- id numbers in my system
    for i=1,network.nodeCount do
        nodes[i] = newNode()
    end
    for i=1,#network.connections do 
        local connection = network.connections[i]
        local outputNode = nodes[connection.outputId]
        table.insert(outputNode.inputConnections, connection)
    end
    return nodes
end

function evaluateNetwork(network)
    local outputCommands = {}

    local nodes = buildNodes(network)
    local inputs = getInputs()
    -- fill in inputs
    for i=1,#inputs do
        nodes[i].value = inputs[i]
    end
    -- last input is bias
    nodes[NumInputs].value = 1
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
    for i=NumInputs+1,NumInputs+NumOutputs do
        local sum = 0
        for j=1,#nodes[i].inputConnections do
            local connection = nodes[i].inputConnections[j]
            if connection.enabled then
                sum = sum + connection.weight * nodes[connection.inputId].value
            end
        end
        nodes[i].value = sigmoid(sum)
        -- write command for controller
        local button = "P1 " .. ButtonNames[i-NumInputs]
        if nodes[i].value > 0 then
            outputCommands[button] = true
        else
            outputCommands[button] = false
        end
    end
	return outputCommands
end

function crossover(network1, network2)
    -- make network 1 the more fit one
    if network1.fitness < network2.fitness then
        local temp = network1
        network1 = network2
        network2 = temp
    end
    -- make a lookup table for the network2's connections
    local innovations2 = {}
    for i=1,#network2.connections do
        local connection = network2.connections[i]
        innovations2[connection.innovationNum] = connection
    end
    -- copy connections from 2 or 1, defaulting to 1 if excess or disjoint
    local childConnections = {}
    for i=1,#network1.connections do
        local connection1 = network1.connections[i]
        if innovations2[connection1.innovationNum] ~= nil and math.random(2) == 1 then
            childConnections[i] = innovations2[connection1.innovationNum]
        else
            childConnections[i] = connection1
        end
    end
    local child = copyNetwork(network1)
    child.connections = childConnections
    child.nodeCount = math.max(network1.nodeCount, network2.nodeCount)
    return child
end

function connectionWeightsMutate(network)
    for i=1,#network.connections do
        local connection = network.connections[i]
        if math.random() < PerturbChance then
            connection.weight = connection.weight + (math.random() - .5) * StepSize
        else
            connection.weight = (math.random() - .5) * 2
        end
    end
end

function addConnectionMutate(network)
    if network.nodeCount < 2 then
        print("NodeCount should always be greater than 1")
        return
    end
    local node1Id = math.random(network.nodeCount)
    local node2Id = math.random(network.nodeCount)
    -- ensure nodes are different and output node is not to an initial input node
    while node1Id == node2Id or node2Id <= NumInputs do
        node2Id = math.random(network.nodeCount)
    end
    local newConnection = newConnection(network)
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

    local connection1 = copyConnection(connection)
    connection1.outputId = network.nodeCount
    connection1.weight = 1.0
    connection1.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection1)

    local connection2 = copyConnection(connection)
    connection2.inputId = network.nodeCount
    connection2.innovationNum = nextInnovationNum(network)
    table.insert(network.connections, connection2)

    connection.enabled = false
    return network
end

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
    enabledConnections[math.random(#enabledConnections)].enabled = false
end

function mutate(network)
    if math.random() < MutateConnectionsChance then
        connectionWeightsMutate(network)
    end
    local p = LinkMutationChance
    while p>0 do
        if math.random() < p then
            addConnectionMutate(network)
        end
        p = p - 1
    end
    if math.random() < NodeMutationChance then
        network = addNodeMutate(network)
    end
    if math.random() < EnableMutationChance then
        enableMutate(network)
    end
    if math.random() < DisableMutationChance then
        disableMutate(network)
    end
    return network
end

-- return the number of disjoint + excess genes between two sets of genes
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
    for i=1,#connections2 do
        if not innovations1[connections2[i].innovationNum] then
            disjointExcessCount = disjointExcessCount + 1
        end
    end
    return disjointExcessCount
end

-- calculates average difference in weights per connections in a network
function avgWeightsDiff(connections1, connections2)
    local sum = 0
    local numDiff = 0
    local innovations2 = {}
    for i=1,#connections2 do
        innovations2[connections2[i].innovationNum] = connections2[i]
    end
    for i=1,#connections1 do
        local connection2 = innovations2[connections1[i].innovationNum]
        if connection2 ~= nil then
            sum = sum + math.abs(connections1[i].weight - connection2.weight)
            numDiff = numDiff + 1
        end
    end
    return sum / numDiff
end
	
-- determine if same species - roughly equivalent of equation 1 in Stanley's paper
function isSameSpecies(network1, network2)
    local disjointDelta = DeltaDisjoint * numDisjointOrExcess(network1.connections, network2.connections)
    local weightDelta = DeltaWeights * avgWeightsDiff(network1.connections, network2.connections)
    return disjointDelta + weightDelta < DeltaThreshold
end

function sortSpeciesNetworks(speciesTable)
    for i=1,#speciesTable do
        -- put best fitnesses at lower indexes
        table.sort(speciesTable[i].networks, function(a,b)
            return (a.fitness > b.fitness)
        end)
        print("Best fitness for species " .. i .. ": " .. speciesTable[i].networks[1].fitness .. " and worst: " .. speciesTable[i].networks[#speciesTable[i].networks].fitness)
    end
end

function calculateAverageFitnesses(speciesTable)
    for i=1,#speciesTable do
        local species = speciesTable[i]
        local sum = 0
        for j=1,#species.networks do
            sum = sum + species.networks[j].fitness
        end
        species.averageFitness = sum/#species.networks
    end
end

-- remove lower 50% of each species
function cullSpeciesNetworks(speciesTable)
    for i=#speciesTable,1 do
        local species = speciesTable[i]
        for j=#species.networks,#species.networks/2+1 do
            if j>1 then
                table.remove(speciesTable, j)
            end
        end
    end
end

-- crossover a child or simply copy 1 and then mutate
function breedChild(species)
    local child = {}
    if math.random() < CrossoverChance then
        network1 = species.networks[math.random(#species.networks)]
        network2 = species.networks[math.random(#species.networks)]
        child = crossover(network1, network2)
    else
        child = copyNetwork(species.networks[math.random(#species.networks)])
    end
    child = mutate(child)
    return child
end

-- add only species to next generation that aren't stale
function removeStaleSpecies(speciesTable)
    for i=#speciesTable,1 do 
        local species = speciesTable[i]
        if species.prevMaxFitness > species.networks[1].fitness then
            species.staleness = species.staleness + 1
            if species.staleness > StaleSpecies then
                table.remove(speciesTable, i)
            end
        else
            species.staleness = 0
            species.prevMaxFitness = species.networks[1].fitness
        end
    end
end

-- sort passed in networks to appropriate species
-- TODO: realized an issue with my species code - new species are made each time so staleness isn't a factor
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

function initNewGeneration()
    -- sort networks in species by fitness
    sortSpeciesNetworks(pool.species)
    -- if fitness didn't improve, add to stale value and remove if stale
    removeStaleSpecies(pool.species)
    -- remove lower 50% of each species
    cullSpeciesNetworks(pool.species)
    -- calculate average fitness of each species
    calculateAverageFitnesses(pool.species)
    -- breed children for new generation - num children is a function of average fitness of that species
    local nextGenNetworks = {}
    local globalAverageFitness = 0
    for i=1,#pool.species do
        globalAverageFitness = globalAverageFitness + pool.species[i].averageFitness
    end
    globalAverageFitness = globalAverageFitness / #pool.species
    for i=1,#pool.species do
        -- TODO: if species starts at fitness below average fitness, speciation won't protect?
        local numChildren = pool.species[i].averageFitness * #pool.species[i].networks / globalAverageFitness
        for j=1,numChildren do
            table.insert(nextGenNetworks, breedChild(pool.species[i]))
        end
    end
    -- breed random children if more room left under population ceiling
    while #nextGenNetworks < NumPopulations do
        local randomSpecies = pool.species[math.random(#pool.species)]
        table.insert(nextGenNetworks ,breedChild(randomSpecies))
    end
    -- configure pool for next gen
    pool.species = {}
    pool.generation = pool.generation + 1
    pool.currentSpecies = 1
    pool.currentNetwork = 1
    pool.currentFrame = 0
    -- sort the new generation's networks into species
    for i=1,#nextGenNetworks do
        addToSpecies(nextGenNetworks[i])
    end
    print("\nGeneration: " .. pool.generation)
end

-- reset control inputs to all false
function clearJoypad()
	controller = {}
	for b = 1,#ButtonNames do
		controller["P1 " .. ButtonNames[b]] = false
	end
	joypad.set(controller)
end

-- returning true if next generation not needed, false if it is
function findNextNetwork(pool)
    local species = pool.species[pool.currentSpecies]
    if species.networks[pool.currentNetwork + 1] ~= nil then
        pool.currentNetwork = pool.currentNetwork + 1
        return true
    elseif pool.species[pool.currentSpecies + 1] ~= nil then
        pool.currentSpecies = pool.currentSpecies + 1
        pool.currentNetwork = 0
        return findNextNetwork(pool)
    else
        return false
    end
end

-- NOTE: END OF NN CODE
-- BEGINNING OF "MAIN"

print(gameinfo.getromname())
initPool()
savestate.load(Filename)
rightmost = 0
timeout = TimeoutConstant
clearJoypad()

while true do
    local species = pool.species[pool.currentSpecies]
    local network = species.networks[pool.currentNetwork]
	-- evaluate network every 5 frames to see next control that should be done
	if pool.currentFrame%5 == 0 then
        controller = evaluateNetwork(network)
    end
    pool.currentFrame = pool.currentFrame + 1
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
        network.fitness = fitness
        print("Fitness: " .. network.fitness .. " for network: " .. pool.currentNetwork .. " of species: " .. pool.currentSpecies)

        local nextNetworkFound = findNextNetwork(pool)
        if not nextNetworkFound then
            initNewGeneration()
        end
        pool.currentFrame = 0
        savestate.load(Filename)
        rightmost = 0
        timeout = TimeoutConstant
        clearJoypad()
    end
    emu.frameadvance()
end