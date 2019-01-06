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

NumPopulations = 100
DeltaDisjoint = 2.0
DeltaWeights = 0.4
DeltaThreshold = 1.0

StaleSpecies = 15

MutateConnectionsChance = 0.25
PerturbChance = 0.90
CrossoverChance = 0.75
LinkMutationChance = 3.0
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
    --[[print("inputs: ")
    for k, v in pairs(inputs) do
        print(k .. " - " .. v)
    end]]
    --[[inputs: 
        1 - 0
        2 - 0
        3 - 0
        4 - 0
        5 - 0
        6 - 0
        7 - 0
        8 - 0
        9 - 0
        10 - 0
        11 - 0
        12 - 0
        13 - 0
        14 - 0
        15 - 0
        16 - 0
        17 - 0
        18 - 0
        19 - 0
        20 - 0
        21 - 0
        22 - 0
        23 - 0
        24 - 0
        25 - 0
        26 - 0
        27 - 0
        28 - 0
        29 - 0
        30 - 0
        31 - 0
        32 - 0
        33 - 0
        34 - 0
        35 - 0
        36 - 0
        37 - 0
        38 - 0
        39 - 0
        40 - 0
        41 - 0
        42 - 0
        43 - 0
        44 - 0
        45 - 0
        46 - 0
        47 - 0
        48 - 0
        49 - 0
        50 - 0
        51 - 0
        52 - 0
        53 - 0
        54 - 0
        55 - 0
        56 - 0
        57 - 0
        58 - 0
        59 - 0
        60 - 0
        61 - 0
        62 - 0
        63 - 0
        64 - 0
        65 - 0
        66 - 0
        67 - 0
        68 - 0
        69 - 0
        70 - 0
        71 - 0
        72 - 0
        73 - 0
        74 - 0
        75 - 0
        76 - 0
        77 - 0
        78 - 0
        79 - 0
        80 - 0
        81 - 0
        82 - 0
        83 - 0
        84 - 0
        85 - 0
        86 - 0
        87 - 0
        88 - 0
        89 - 0
        90 - 0
        91 - 0
        92 - 0
        93 - 0
        94 - 0
        95 - 0
        96 - 0
        97 - 0
        98 - 0
        99 - 0
        100 - 0
        101 - 0
        102 - 0
        103 - 0
        104 - 0
        105 - 0
        106 - 0
        107 - 0
        108 - 0
        109 - 0
        110 - 1
        111 - 1
        112 - 1
        113 - 1
        114 - 1
        115 - 1
        116 - 1
        117 - 1
        118 - 0
        119 - 0
        120 - 0
        121 - 0
        122 - 0
        123 - 0
        124 - 0
        125 - 0
        126 - 0
        127 - 0
        128 - 0
        129 - 0
        130 - 0
        131 - 0
        132 - 0
        133 - 0
        134 - 0
        135 - 0
        136 - 0
        137 - 0
        138 - 0
        139 - 0
        140 - 0
        141 - 0
        142 - 0
        143 - 0
        144 - 0
        145 - 0
        146 - 0
        147 - 0
        148 - 0
        149 - 0
        150 - 0
        151 - 0
        152 - 0
        153 - 0
        154 - 0
        155 - 0
        156 - 0
        157 - 0
        158 - 0
        159 - 0
        160 - 0
        161 - 0
        162 - 0
        163 - 0
        164 - 0
        165 - 0
        166 - 0
        167 - 0
        168 - 0
        169 - 0]]
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

-- arrays in Lua begin with 1
function initPool()
    pool = {}
    pool.species = {}
    pool.generation = 0
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
    local newNetwork = {}
    newNetwork.connections = network.connections
    newNetwork.nodeCount = network.nodeCount
    newNetwork.fitness = network.fitness
    newNetwork.innovationNum = network.innovationNum
    return newNetwork
end

function newConnection(network)
    -- print("newConnection")
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
                -- print("connection.inputId: " .. connection.inputId)
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
        --print("outputs sum: " .. sum)
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
    return child
end

-- if mutating connections, change gene weights in genome
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

-- add a new connection to the genome between two random nodes
function addConnectionMutate(network)
    if network.nodeCount < 2 then
        print("nodeCount should always be greater than 1")
        return
    end
    local node1Id = math.random(network.nodeCount)
    local node2Id = math.random(network.nodeCount)
    -- ensure nodes are different and output node is not to an initial input node
    while node1Id == node2Id or node2Id < NumInputs do
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
    -- print("addNodeMutate")
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
    --print("mutating network: " .. network.nodeCount)
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
        addNodeMutate(network)
    end
    if math.random() < EnableMutationChance then
        enableMutate(network)
    end
    if math.random() < DisableMutationChance then
        disableMutate(network)
    end
    return network
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
    for i=1,#connections2 do
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
    for i=1,#connections1 do
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
    local weightDelta = DeltaWeights * avgWeightsDiff(network1.connections, network2.connections)
    return disjointDelta + weightDelta < DeltaThreshold
end

-- rank all genomes regardless of species
function sortSpeciesNetworks(speciesTable)
    for i=1,#speciesTable do
        -- put best fitnesses at lower indexes
        table.sort(speciesTable[i].networks, function(a,b)
            return (a.fitness > b.fitness)
        end)
        print("best fitness for species " .. i .. ": " .. speciesTable[i].networks[1].fitness)
        print("worst fitness for species " .. i .. ": " .. speciesTable[i].networks[#speciesTable[i].networks].fitness)
    end
end

-- calculate average rank of a species
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
    for i=1,#speciesTable do
        local species = speciesTable[i]
        -- TODO: ATTEMPT TO ACCESS NIL VALUE SPECIES?!? - happened because no species improved and all were removed for being stale?
        for j=#species.networks,#species.networks/2+1 do
            table.remove(speciesTable, j)
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
    mutate(child)
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
        -- num children = total fitness/average fitness of all species
    local nextGenNetworks = {}
    local globalAverageFitness = 0
    for i=1,#pool.species do
        globalAverageFitness = globalAverageFitness + pool.species[i].averageFitness
    end
    globalAverageFitness = globalAverageFitness / #pool.species
    for i=1,#pool.species do
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
        print("found next network: " .. pool.currentNetwork .. " in species: " .. pool.currentSpecies)
        return true
    elseif pool.species[pool.currentSpecies + 1] ~= nil then
        pool.currentSpecies = pool.currentSpecies + 1
        return findNextNetwork(pool)
    else
        return false
    end
end

-- NOTE: END OF NN CODE
-- BEGINNING OF "MAIN()"

--INITIALIZE RUN CODE - TODO - REWORK!
initPool()
savestate.load(Filename)
rightmost = 0
timeout = TimeoutConstant
clearJoypad()

-- a lot of gui updating code, but also handles running neural network/ indirectly updating when network has finished its run
while true do
    local species = pool.species[pool.currentSpecies]
    local network = species.networks[pool.currentNetwork]
	-- evaluate network every 5 frames to see next control that should be done
	if pool.currentFrame%5 == 0 then
        -- get controls neural network says to use
        controller = evaluateNetwork(network)
        --[[print("controller: ")
        for k, v in pairs(controller) do
            print(k .. " - " .. tostring(v))
        end]]
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
		-- fitness is the indicator of where the program is currently executing in this weird loop
        network.fitness = fitness
        print("Fitness: " .. network.fitness)
		-- reset currentspecies and current genome and increment them until you find a non measure genome; once found, reset for new run
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