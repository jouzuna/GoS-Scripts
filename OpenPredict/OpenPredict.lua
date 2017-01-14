if _G.OpenPredict_Version then return end

local localHero = GetMyHero()

local TEAM_NONE = 0
local TEAM_ALLY = GetTeam(localHero)
local TEAM_ENEMY = 300 - TEAM_ALLY -- Inspired by nebelwaffle
local TEAM_NEUTRAL = 300
local TEAM_ENEMY = TEAM_NEUTRAL - TEAM_ALLY

-- PATH MANAGER
local PathManager = { }
PathManager.path = { }
PathManager.samples = { }

function PathManager.ProcessWaypoint(unit, waypointProc)
	local nID = GetNetworkID(unit)

	if not PathManager.path[nID] or GetGameTimer() > PathManager.path[nID].pathCreationTime then
		-- New path creation
		PathManager.path[nID] = {
			pathCreationTime = GetGameTimer(),
			prevDirection = GetDirection(unit),
			--[[
			Lua length(#) operator does not work if not inserting/
			removing from table correctly, must keep count manually
			]]maxIndex = waypointProc.index
		}
	end

	local path = PathManager.path[nID] -- Create pointer to table
	path[waypointProc.index] = waypointProc.position
	path[waypointProc.index].dashSpeed = waypointProc.dashspeed or 0

	-- Add sample data
	if not PathManager.samples[nID] then
		PathManager.samples[nID] = { }
	end

	if waypointProc.index == 1 and GetObjectType(unit) == "AIHeroClient" then
		local samples = PathManager.samples[nID]
		table.insert(samples, 1, GetGameTimer())

		if #samples > 10 then
			table.remove(samples)
		end
	end
end

function PathManager:GetPath(unit)
	local nID = GetNetworkID(unit)

	if self.path[nID] then
		local path = self.path[nID] -- Create ptr to path for faster access
		local curIndex = 0

		-- Find the current waypoint index
		local origin = GetOrigin(unit)

		for k = path.maxIndex, 1, -1 do
			if path[k] and path[k - 1] and (origin.x - path[k].x) ^ 2 + (origin.z - path[k].z) ^ 2 < (path[k - 1].x - path[k].x) ^ 2 + (path[k - 1].z - path[k].z) ^ 2 then
				curIndex = path.maxIndex - k
				break
			end
		end

		-- Return path iterator
		return function()
			curIndex = curIndex + 1

			if curIndex < path.maxIndex and path[path.maxIndex - curIndex] then
				return curIndex, path[path.maxIndex - curIndex]
			end
		end
	end

	return function() end
end

function PathManager:CalculateInterceptionPoint(unit, delay, speed, sourcePos)
	local prevWaypoint = GetOrigin(unit)
	local px, py = prevWaypoint.x, prevWaypoint.z -- Return values
	local timeElapsed = 0

	for index, waypoint in self:GetPath(unit) do
		-- Calculate the time needed to travel entire waypoint
		local velocity = waypoint.dashSpeed > 0 and waypoint.dashSpeed or GetMoveSpeed(unit)
		local dx, dy = waypoint.x - prevWaypoint.x, waypoint.z - prevWaypoint.z
		local magnitude = math.sqrt(dx * dx + dy * dy)
		local travelTime = magnitude / velocity

		-- Normalize to unit vectors and calculate the velocity vectors
		dx, dy = dx / magnitude, dy / magnitude
		local vx, vy = dx * velocity, dy * velocity

		-- Realculate the start point
		local sx, sy = prevWaypoint.x - vx * timeElapsed, prevWaypoint.z - vy * timeElapsed

		local t = 0 -- Projectile collision time
		if speed ~= math.huge then
			-- Calculate interception time
			local a = (vx * vx) + (vy * vy) - (speed * speed)
			local b = 2 * ((sx * vx) + (sy * vy) - (sourcePos.x * vx) - (sourcePos.z * vy))
			local c = (sx * sx) + (sy * sy) + (sourcePos.x * sourcePos.x) + (sourcePos.z * sourcePos.z) - (2 * sourcePos.x * sx) - (2 * sourcePos.z * sy)
			local disc = (b * b) - (4 * a * c)

			local t1 = (-b + math.sqrt(disc)) / (2 * a)
			local t2 = (-b - math.sqrt(disc)) / (2 * a)

			-- Greater of the two roots
			t = math.max(t1, t2)
		end

		if delay + t > timeElapsed and delay + t < timeElapsed + travelTime then
			-- Projectile will collide on current waypoint
			px = sx + vx * (delay + t)
			py = sy + vy * (delay + t)
			break
		end

		-- End of iteration, update static values for next iter
		timeElapsed = timeElapsed + travelTime
		prevWaypoint = waypoint
		px, py = waypoint.x, waypoint.z
	end

	return px, py
end

function PathManager:GetPathAtIndex(unit, index)
	local nID = GetNetworkID(unit)

	if self.path[nID] then
		return self.path[nID][self.path[nID].maxIndex - (index < 0 and -index or index)] or GetOrigin(unit)
	end
end

function PathManager:GetPathFrequency(unit, timespan)
	local nID = GetNetworkID(unit)

	if self.samples[nID] then
		local samples = self.samples[nID]
		local frequency = 0

		for i = 1, #samples, 1 do
			if GetGameTimer() - samples[i] <= timespan then
				frequency = frequency + 1
			end
		end

		return frequency
	end

	return 0
end

function PathManager:GetPreviousPathDirection(unit)
	local nID = GetNetworkID(unit)
	return self.path[nID] and self.path[nID].prevDirection or GetDirection(unit)
end

_G.OnProcessWaypoint(PathManager.ProcessWaypoint)

-- OBJECT MANAGER
local ObjectManager = { }
ObjectManager.objectQueue = { } -- Stores uninitialized objects
ObjectManager.gameHeroes = { }
ObjectManager.gameMinions = { }
ObjectManager.gameObjects = { -- Pointer table
	["AIHeroClient"] = ObjectManager.gameHeroes,
	["obj_AI_Minion"] = ObjectManager.gameMinions
}

function ObjectManager.ProcessQueue()
	for i = #ObjectManager.objectQueue, 1, -1 do
		local object = ObjectManager.objectQueue[i]
		local nID = GetNetworkID(object)

		if nID and nID > 0 and nID < math.huge then
			local team = GetTeam(object)

			if team and team > 0 and team % 100 == 0 then
				local objectType = GetObjectType(object)

				if ObjectManager.gameObjects[objectType] then
					table.insert(ObjectManager.gameObjects[objectType], object)
				end
			end

			table.remove(ObjectManager.objectQueue, i)
		end
	end
end

function ObjectManager.ProcessObject(object)
	if ObjectManager.gameObjects[GetObjectType(object)] then
		table.insert(ObjectManager.objectQueue, object)
	end
end

function ObjectManager.RemoveObject(object)
	local type = GetObjectType(object)

	if ObjectManager.gameObjects[type] then
		local objects = ObjectManager.gameObjects[type]

		for i = 1, #objects do
			if objects[i] == object then
				table.remove(objects, i)
				break
			end
		end
	end
end

function ObjectManager.LostVision(object)

end

function ObjectManager:GetHeroes(filterTeam)
	local heroes = self.gameObjects["AIHeroClient"]
	local index, count = 0, #self.gameObjects["AIHeroClient"]

	return function()
		repeat
			index = index + 1

			if heroes[index] and IsVisible(heroes[index]) and IsObjectAlive(heroes[index]) then
				local team = GetTeam(heroes[index])

				if (not filterTeam or filterTeam == 0) or (team == filterTeam or team == 300) then
					return heroes[index]
				end
			end
		until(index > count)
	end
end

function ObjectManager:GetMinions(filterTeam)
	local minions = self.gameObjects["obj_AI_Minion"]
	local index, count = 0, #self.gameObjects["obj_AI_Minion"]

	return function()
		repeat
			index = index + 1

			if minions[index] and IsVisible(minions[index]) and IsObjectAlive(minions[index]) then
				local team = GetTeam(minions[index])

				if (not filterTeam or filterTeam == 0) or (team == filterTeam or team == filterTeam - 300 or team == filterTeam - (filterTeam - 300)) then
					return minions[index]
				end
			end
		until(index > count)
	end
end

_G.OnTick(ObjectManager.ProcessQueue)
_G.OnObjectLoad(ObjectManager.ProcessObject)
_G.OnCreateObj(ObjectManager.ProcessObject)
_G.OnDeleteObj(ObjectManager.RemoveObject)
_G.OnLoseVision(ObjectManager.LoseVision)

-- ATTACK HANDLER
local AttackHandler = { }
AttackHandler.attackBySource = { }
AttackHandler.missileSpeed = { }

function AttackHandler.OnCreateObj(object)
	if GetObjectType(object) == "MissileClient" then
		local spellName = GetObjectSpellName(object)

		if not AttackHandler.missileSpeed[spellName] then
			AttackHandler.missileSpeed[spellName] = GetObjectMissileSpeed(object)
		end
	end
end

function AttackHandler.OnProcessSpellAttack(unit, attackProc)
	AttackHandler.attackBySource[GetNetworkID(unit)] = {
		source = unit,
		target = attackProc.target,
		startTime = GetGameTimer(),
		windUpTime = attackProc.windUpTime,
		missileSpeed = AttackHandler.missileSpeed[attackProc.name] or math.huge,
		animTime = attackProc.animationTime
	}
end

function AttackHandler.OnAttackCancel(unit)
	AttackHandler.attackBySource[GetNetworkID(unit)] = nil
end

_G.OnCreateObj(AttackHandler.OnCreateObj)
_G.OnProcessSpellAttack(AttackHandler.OnProcessSpellAttack)
_G.OnAttackCancel(AttackHandler.OnAttackCancel)

-- MOBILITY HANDLER
local MobilityHandler = { }
MobilityHandler.layer = { }
MobilityHandler.buffType = {
	BUFF_Internal = 0,
	BUFF_Aura = 1,
	BUFF_CombatEnchancer = 2,
	BUFF_CombatDehancer = 3,
	BUFF_SpellShield = 4,
	BUFF_Stun = 5,
	BUFF_Invisibility = 6,
	BUFF_Silence = 7,
	BUFF_Taunt = 8,
	BUFF_Polymorph = 9,
	BUFF_Slow = 10,
	BUFF_Snare = 11,
	BUFF_Damage = 12,
	BUFF_Heal = 13,
	BUFF_Haste = 14,
	BUFF_SpellImmunity = 15,
	BUFF_PhysicalImmunity = 16,
	BUFF_Invulnerability = 17,
	BUFF_AttackSpeedSlow = 18,
	BUFF_NearSight = 19,
	BUFF_Fear = 20,
	BUFF_Charm = 21,
	BUFF_Poison = 22,
	BUFF_Suppression = 23,
	BUFF_Blind = 24,
	BUFF_Counter = 25,
	BUFF_Currency = 26,
	BUFF_Shred = 27,
	BUFF_Flee = 28,
	BUFF_Knockup = 29,
	BUFF_Knockback = 30,
	BUFF_Disarm = 31,
	BUFF_Grounded = 32
	-- Table added for readability
}

local function OnUpdateBuff(unit, buff)
	local nID = GetNetworkID(unit)
	local type = buff.Type

	if buff.ExpireTime > (MobilityHandler.layer[nID] or 0) and (buff.type == 5 or type == 8 or type == 11 or type == 20 or type == 21 or type == 23 or type == 28 or type == 29 or type == 30) then
		MobilityHandler.layer[nID] = buff.ExpireTime
	end
end

_G.OnUpdateBuff(OnUpdateBuff)

-- BARRIER HANDLER
local BarrierHandler = { }
BarrierHandler.windWall = { }
BarrierHandler.unbreakable = { }

function BarrierHandler.OnCreateObj(object)
	local spellName = GetObjectSpellName(object)

	if spellName:find("YasuoWMovingWallMis") then
		local associativeIndex = spellName:sub(20, -1)
		-- Scan table to see if wall structure already exists
		for i = 1, #BarrierHandler.windWall, 1 do
			if BarrierHandler.windWall[i].startTime == GetGameTimer() then
				BarrierHandler.windWall[i][associativeIndex] = object
				return
			end
		end

		table.insert(BarrierHandler.windWall, {
			startTime = GetGameTimer(),
			[associativeIndex] = object
		})
	end
end

function BarrierHandler.OnDeleteObj(object)
	local spellName = GetObjectSpellName(object)

	if spellName:find("YasuoWMovingWallMis") then
		for i = 1, #BarrierHandler.windWall, 1 do
			if BarrierHandler.windWall[i][spellName:sub(20, -1)] == object then
				table.remove(BarrierHandler.windWall, i)
				break
			end
		end
	end
end

function BarrierHandler.OnProcessSpellCast(unit, spellProc)
	if spellProc.name == "BraumE" then
		local dx, dy = spellProc.endPos.x - spellProc.startPos.x, spellProc.endPos.z - spellProc.startPos.z
		local m = math.sqrt(dx * dx + dy * dy)

		table.insert(BarrierHandler.unbreakable, {
			startTime = GetGameTimer(),
			endTime = GetGameTimer() + 2.75 + 0.25 * GetCastLevel(unit, 2),
			braum = unit,
			direction = { x = dx / m, y = dy / m }
		})
	end
end

_G.OnCreateObj(BarrierHandler.OnCreateObj)
_G.OnDeleteObj(BarrierHandler.OnDeleteObj)
_G.OnProcessSpellCast(BarrierHandler.OnProcessSpellCast)

-- CORE
local predictInfo = { }
predictInfo.__index = predictInfo

function predictInfo.new(predictPos, targetUnit, metaData, sourcePos)
	local pI = { }
	setmetatable(pI, predictInfo)

	pI.x, pI.y, pI.z = predictPos.x, predictPos.y, predictPos.z
	pI.castPos = { x = predictPos.x, y = predictPos.y, z = predictPos.z }
	pI.hitChance = 0
	pI.timeToHit = 0
	pI.targetUnit = targetUnit
	pI.meta = { }
	pI.sourcePos = { x = sourcePos.x, y = sourcePos.y, z = sourcePos.z }

	for key, value in pairs(metaData) do
		pI.meta[key] = value
	end

	return pI
end

function predictInfo:setPosition(x, y)
	self.x = x
	self.z = y
	self.castPos.x = x
	self.castPos.z = y
end

function predictInfo:hCollision(minCollisionUnits, team)
	local collision = { }
	local minCollisionUnits = minCollisionUnits or math.huge
	local HeroNext = ObjectManager:GetHeroes(team or GetTeam(self.targetUnit))

	repeat
		local hero = HeroNext()
		if hero == nil then break end

		if hero ~= self.targetUnit and self:checkCollision(hero) then
			table.insert(collision, hero)
		end
	until(minCollisionUnits > 0 and #collision >= minCollisionUnits)

	return #collision >= minCollisionUnits and collision
end

function predictInfo:mCollision(minCollisionUnits, team)
	local collision = { }
	local minCollisionUnits = minCollisionUnits or math.huge
	local MinionNext = ObjectManager:GetMinions(team or GetTeam(self.targetUnit))

	repeat
		local minion = MinionNext()
		if minion == nil then break end

		if minion ~= self.targetUnit and self:checkCollision(minion) then
			table.insert(collision, minion)
		end
	until(minCollisionUnits > 0 and #collision >= minCollisionUnits)

	return #collision >= minCollisionUnits and collision
end

function predictInfo:pCollision()
	local gameTimer = GetGameTimer()

	-- UNBREAKABLE
	local unbreakable = BarrierHandler.unbreakable
	local dx, dy = self.castPos.x - self.sourcePos.x, self.castPos.z - self.sourcePos.z
	local m = math.sqrt(dx * dx + dy * dy)

	for i = 1, #unbreakable, 1 do
		-- Check if Braum's shield is facing the projectile
		local dir = unbreakable[i].direction
		if (dx / m) * dir.x + (dy / m) * dir.y < 0 then
			local t = self:checkCollision(unbreakable[i].braum)
			return gameTimer + t < unbreakable[i].endTime -- Return if the time of collision is before the shield expires
		end
	end

	-- WIND WALL
	local windWallArray = BarrierHandler.windWall
	local p1, p2 = self.sourcePos, self.castPos
	local intersectDistance = { ["L"] = math.huge, ["R"] = math.huge }

	for i = 1, #windWallArray, 1 do
		local windWall = windWallArray[i]

		if windWall["L"] and windWall["R"] then
			-- Find the closest side intersection
			for k in pairs(intersectDistance) do
				local p3, p4 = GetOrigin(windWall[k]), GetObjectSpellEndPos(windWall[k])
				local c = (p1.x - p2.x) * (p3.z - p4.z) - (p1.z - p2.z) * (p3.x - p4.x)

				if math.abs(c) >= 1e-9 then
					local a = p1.x * p2.z - p1.z * p2.x
					local b = p3.x * p4.z - p3.z * p4.x

					local x = (a * (p3.x - p4.x) - b * (p1.x - p2.x)) / c
					local y = (a * (p3.z - p4.z) - b * (p1.z - p2.z)) / c

					intersectDistance[k] = (x - p1.x) ^ 2 + (y - p1.z) ^ 2
				end
			end

			local t = self.meta["delay"] + math.sqrt(math.min(intersectDistance["L"], intersectDistance["R"])) / self.meta["speed"]

			-- Calculate pseudo position for the wall
			local sP, eP = GetObjectSpellStartPos(windWall["Vis"]), GetOrigin(windWall["Vis"])
			local ux, uy = eP.x - sP.x, eP.z - sP.z
			local m = math.sqrt(dx * dx + dy * dy)
			ux, uy = ux / m, uy / m

			local t1 = m / 850
			local p3, p4 = GetObjectSpellStartPos(windWall["L"]), GetObjectSpellStartPos(windWall["R"])
			if t1 + t < 0.25 then
				-- Prediction rarely occurs here
				p3.x, p3.x = p3.x + ux * (t1 * 850), p3.z + uy * (t1 * 850)
				p4.x, p4.z = p4.x + ux * (t1 * 850), p4.z + uy * (t1 * 850)
			else
				local castDisplacement, driftSpeed = 0.25 * 850, 340 / 4
				local t2 = (m - castDisplacement) / driftSpeed + t
				local dx, dy = ux * castDisplacement + ux * (t2 * driftSpeed), uy * castDisplacement + uy * (t2 * driftSpeed)
				p3.x, p3.z = p3.x + dx, p3.z + dy
				p4.x, p4.z = p4.x + dx, p4.z + dy
			end

			-- Check for intersection on wall
			local x1, x2, y1, y2 = p2.x - p1.x, p4.x - p3.x, p2.z - p1.z, p4.z - p3.z
			local denom = -x2 * y1 + x1 * y2
			local s = (-y1 * (p1.x - p3.x) + x1 * (p1.z - p3.z)) / denom

			if s >= 0 and s <= 1 then
				local tx = (x2 * (p1.z - p3.z) - y2 * (p1.x - p3.x)) / denom

				if tx >= 0 and tx <= 1 then
					local m = math.sqrt((tx * x1) ^ 2 + (tx * y1) ^ 2)
					return self.meta["delay"] + m / self.meta["speed"] < self.timeToHit
				end
			end
		end
	end

	return false
end

function predictInfo:checkCollision(unit)
	local sP1, sP2 = self.sourcePos, GetOrigin(unit)
	local eP1, eP2 = self.castPos, PathManager:GetPathAtIndex(unit, -1) or sP2

	local a, b, c
	local pathTravelTime = math.huge

	if math.abs(eP2.x - sP2.x + eP2.z - sP2.z) > 1e-9 then
		local s1, s2 = self.meta["speed"], GetMoveSpeed(unit)

		local vx1, vy1 = eP1.x - sP1.x, eP1.z - sP1.z -- Velocity of the projectile
		local vx2, vy2 = eP2.x - sP2.x, eP2.z - sP2.z -- Velocity of the unit

		local m1 = math.sqrt(vx1 * vx1 + vy1 * vy1)
		local m2 = math.sqrt(vx2 * vx2 + vy2 * vy2)

		pathTravelTime = m2 / s2

		vx1, vy1 = (vx1 / m1) * s1, (vy1 / m1) * s1
		vx2, vy2 = (vx2 / m2) * s2, (vy2 / m2) * s2

		-- Extend the unit start position to match the cast delay
		sP2.x = sP2.x + vx2 * (self.meta["delay"] or 0)
		sP2.z = sP2.z + vy2 * (self.meta["delay"] or 0)

		a = (vx1 - vx2) ^ 2 + (vy1 - vy2) ^ 2
		b = 2 * ((sP1.x - sP2.x) * (vx1 - vx2) + (sP1.z - sP2.z) * (vy1 - vy2))
		c = (sP1.x - sP2.x) ^ 2 + (sP1.z - sP2.z) ^ 2 - (GetHitBox(unit) + self.meta["width"]) ^ 2
	else
		a = (eP1.x - sP1.x) ^ 2 + (eP1.z - sP1.z) ^ 2
		b = 2 * ((sP1.x - eP2.x) * (eP1.x - sP1.x) + (sP1.z - eP2.z) * (eP1.z - sP1.z))
		c = (sP1.x - eP2.x) ^ 2 + (sP1.z - eP2.z) ^ 2 - (GetHitBox(unit) + self.meta["width"]) ^ 2
	end

	local disc = b * b - 4 * a * c

	if disc >= 0 then
		local t1 = (-b + math.sqrt(disc)) / (2 * a)
		local t2 = (-b - math.sqrt(disc)) / (2 * a)
		local t = self.meta["delay"] + math.min(t1, t2)

		return t > 0 and t < self.timeToHit and t < pathTravelTime and t
	end
end

function GetPrediction(unit, spellData, sourcePos)
	-- Fail-safe conversions
	local delay = spellData.delay or 0
	local speed = spellData.speed or math.huge
	local width = spellData.width or (spellData.radius and 2 * spellData.radius) or 1
	local range = spellData.range or math.huge
	local source = sourcePos or GetOrigin(localHero)

	local origin = GetOrigin(unit)
	local nID = GetNetworkID(unit)
	-- Create the return object
	local predictInfo = predictInfo.new(origin, unit, spellData, source)
	predictInfo:setPosition(
		PathManager:CalculateInterceptionPoint(unit, delay, speed, source)
	)

	local distanceToHit = math.sqrt((predictInfo.castPos.x - source.x) ^ 2 + (predictInfo.castPos.z - source.z) ^ 2)
	local timeToHit = delay + distanceToHit / speed

	-- Calculate the angle needed to rotate
	local dx, dy = predictInfo.x - source.x, predictInfo.z - source.z
	local theta = (math.atan2(predictInfo.z - origin.z, predictInfo.x - origin.x) - math.atan2(dy, dx)) % (2 * math.pi)
	local phi = math.atan2(GetHitBox(unit) + width - 2, speed * timeToHit) * (1 - math.abs(theta % math.pi - 0.5 * math.pi) / (0.5 * math.pi))
	phi = theta < math.pi and -phi or phi

	-- Rotate the vector
	local c, s = math.cos(phi), math.sin(phi)
	predictInfo.castPos.x = source.x + c * dx - s * dy
	predictInfo.castPos.z = source.z + s * dx + c * dy

	-- Calculate the hit probability
	local distanceToEvade = GetHitBox(unit) + 0.5 * width-- + math.sqrt((predictInfo.x - predictInfo.castPos.x) ^ 2 + (predictInfo.z - predictInfo.castPos.z) ^ 2)
	local timeToEvade = distanceToEvade / GetMoveSpeed(unit) + math.max(0, (MobilityHandler.layer[nID] or 0) - GetGameTimer())

	local modifier = 0.5
	local attack = AttackHandler.attackBySource[nID]
	if attack and GetGameTimer() < attack.startTime + attack.windUpTime then
		modifier = modifier + attack.windUpTime
	elseif not attack or GetGameTimer() > attack.startTime + attack.animTime then
		local pathFrequency = PathManager:GetPathFrequency(unit, timeToHit)

		if pathFrequency > 1 then
			local prevDirection = PathManager:GetPreviousPathDirection(unit)
			local lastWaypoint = PathManager:GetPathAtIndex(unit, -1)

			local dx, dy = lastWaypoint.x - origin.x, lastWaypoint.z - origin.z
			local waypointLength = math.sqrt(dx * dx + dy * dy)

			local c = (dx / waypointLength) * prevDirection.x + (dy / waypointLength) * prevDirection.z
			if c == c then
				modifier = modifier + c * (0.5 - (timeToEvade / pathFrequency))
			end
		end
	end

	predictInfo.hitChance = distanceToHit < range and math.min(0.99, ((timeToEvade * modifier) / timeToHit)) or 0
	predictInfo.timeToHit = timeToHit

	return predictInfo
end

function GetCircularAOEPrediction(...)
	return GetPrediction(...)
end
function GetLinearAOEPrediction(...)
	return GetPrediction(...)
end
function GetConicAOEPrediction(...)
	return GetPrediction(...)
end

function GetHealthPrediction(unit, timespan)
	local totalDamage, aggroCount = 0, 0
	local p1 = GetOrigin(unit)

	for nID, attack in pairs(AttackHandler.attackBySource) do
		if attack.target == unit and (IsObjectAlive(attack.source) or GetGameTimer() > attack.startTime + attack.windUpTime) then
			-- If the source died and the projectile has already been created, the attack still persists
			local p2 = GetOrigin(attack.source)
			local distance = math.sqrt((p2.x - p1.x) ^ 2 + (p2.z - p1.z) ^ 2)
			local timeToHit = attack.windUpTime + distance / attack.missileSpeed

			local startTime = attack.startTime + math.floor((GetGameTimer() - attack.startTime) / attack.animTime)
			if GetGameTimer() + timespan > startTime + timeToHit then
				totalDamage = totalDamage + GetBaseDamage(attack.source)
			end
		end
	end

	return GetCurrentHP(unit) - totalDamage
end

function OpenPredictExportAll(environment)
	local env = environment or _ENV

	env["PathManager"] = PathManager
	env["ObjectManager"] = ObjectManager
	env["GetPrediction"] = GetPrediction
	env["GetCircularAOEPrediction"] = GetCircularAOEPrediction
	env["GetLinearAOEPrediction"] = GetLinearAOEPrediction
	env["GetConicAOEPrediction"] = GetConicAOEPrediction
	env["GetHealthPrediction"] = GetHealthPrediction
end

_G.OpenPredict_Version = "0.06b"
PrintChat("<font color=\"#FFFFFF\"><b>OpenPredict</b> " .. OpenPredict_Version .. " loaded!</font>")
