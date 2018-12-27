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
InputSize = (BoxRadius*2+1)*(BoxRadius*2+1)

Inputs = InputSize+1
Outputs = #ButtonNames

Population = 300
DeltaDisjoint = 2.0
DeltaWeights = 0.4
DeltaThreshold = 1.0

StaleSpecies = 15

MutateConnectionsChance = 0.25
PerturbChance = 0.90
CrossoverChance = 0.75
LinkMutationChance = 2.0
NodeMutationChance = 0.50
BiasMutationChance = 0.40
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

function newInnovation()
    -- TODO
end

function newPool()
	-- TODO
end

function newSpecies()
	-- TODO
end

function newGenome()
	-- TODO
end

function copyGenome(genome)
	-- TODO
end

function basicGenome()
	-- TODO
end

function newGene()
	-- TODO
end

function copyGene(gene)
	-- TODO
end

function newNeuron()
	-- TODO
end

function generateNetwork(genome)
	-- TODO
end

function evaluateNetwork(network, inputs)
	-- TODO
end

function crossover(g1, g2)
	-- TODO
end

function randomNeuron(genes, nonInput)
	-- TODO
end

--return if connection exists, regardless of weights
function containsLink(genes, link)
	-- TODO
end

-- if mutating connections, change gene weights in genome
function pointMutate(genome)
	-- TODO
end

-- add a new connection to the genome from an input to a random node
function linkMutate(genome, forceBias)
	-- TODO
end

-- mutate a single connection to become two connections with a node inbetween that is mathematically identical for the current run
function nodeMutate(genome)
	-- TODO
end

-- mutate to either enable or disable a connection - which action to take is determined by parameter
function enableDisableMutate(genome, enable)
	-- TODO
end

function mutate(genome)
	-- TODO
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
function nextGenome()
	-- TODO
end

function fitnessAlreadyMeasured()
	-- TODO
end

-- TODO: END OF NN CODE

-- a lot of gui updating code, but also handles running neural network/ indirectly updating when network has finished its run
while true do
	-- TODO
end