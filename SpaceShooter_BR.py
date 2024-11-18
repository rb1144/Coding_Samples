'''
******* SPACE SHOOTER ********
bogdan romocea, 2019-02 and later
This is a space shooter game implemented in the Pythonista iOS app, using the scene module which provides an easy way to create 
hardware-accelerated 2D games. To my knowledge, the scene module was written specifically for Pythonista and has not been ported 
elsewhere. But if you have that app, open this file there and the game will run.

Game rules
1. Tilt your device to control the movement and speed of the ship, and touch the screen to shoot.
2. Do not crash into meteors. If shooting them, some will contain coins, bolts or UFOs. The UFOs can shoot their own lasers at you.
3. Smaller meteors contain more valuable coins or bolts (gold or silver) that bring more points. Each laser hit causes a damage of 1. 
4. Collect coins & bolts and destroy UFOs to score points.
5. The Gold and Silver bolts will explode all meteors and bring 5 respectively 3 points each. The Bronze bolt is perhaps the best,
   as it provides an extra laser for the ship (up to 5 total).
'''

#---Gameplay settings. Adjust these to make the game easier or harder
meteorsPerSec = 1.5  #meteors per second, on average
meteorFillPct = 0.30  #the proportion of meteors that contain a coin, bolt or UFO
meteorDuration = (4, 10)  #Meteor duration on screen (range). Shorter durations = higher speeds
lasersPerSec_UFO = 0.4  #how many lasers per second the UFOs can shoot
destroy_UFO_pts = 50  #how many points to award for destroying an UFO
hits2Explode = {'Big': 2, 'Med': 1, 'UFO': 10}  #number of laser hits required to explode meteors and UFOs


import scene as sc
#import copy
import datetime
import math
import os
#import objc_util
import random
import sound
import time

A = sc.Action  #save typing later
ss = sc.get_screen_size()  #the number of pixels on the X and Y axes

#---The objects: meteor types + sizes, ships types + sizes, and UFO colors
sizes = [s + n for s in ['Big','Med','Small','Tiny'] for n in ['1','2']] + ['Big3','Big4']
metype = ['spc:Meteor' + c + s for c in ['Brown','Gray'] for s in sizes]
#random.choices(mets, k=25)
#random.choice(mets)
ships = ['spc:PlayerShip' + n + c for n in ['1','2','3'] for c in ['Red','Blue','Orange','Green']]
ufos = ['spc:UFO' + c for c in ['Red','Blue','Yellow','Green']]


class MyGame (sc.Scene):

	#Initialize the scene objects: screen color, ship, items, score and new game	
	def setup(self):
		#print(os.getcwd())
		self.background_color = .11, .39, .92
		self.ship = sc.SpriteNode(random.choice(ships), z_position=1)
		self.items = []
		self.score = 0
		self.score_label = sc.LabelNode(str(self.score), ('Futura', 40), parent=self, position=(self.size.w/2, self.size.h - 40), z_position=1)
		self.new_game()


	#This function is called once per frame by the game engine. This is the place to update what happens in the game			
	def update(self):
		if self.ship.destroyed:
			return
		x, y, z = sc.gravity()  #determine the orientation of the device
		pos = self.ship.position
		pos += (x * 50, y * 50)
		# Don't allow the ship to move beyond the screen bounds:
		pos.x = max(0, min(self.size.w, pos.x))
		pos.y = max(50, min(self.size.h, pos.y))
		#print(posm)
		self.ship.position = pos
		#print(self.ship.frame)
		self.new_meteor()
		self.laser_hits()
		self.ship_collisions()
		#print(str(len(self.items)) + ' items, ' + str(len(self.lasers)) + ' lasers')
		#print(str(len(self.lasers)) + ' lasers')


	#When starting a new game, reset everything to the initial state: no items, no lasers, score=0, ship at default position, etc.
	def new_game(self):
		# Reset everything to its initial state
		for m in self.items:
			m['node'].remove_from_parent()
		self.items = []
		self.lasers = []
		self.laser_at = [0]
		self.ship = sc.SpriteNode(random.choice(ships), z_position=1)
		self.ship.position = [ss[0] / 2, 40]
		self.add_child(self.ship)
		self.ship.destroyed = False
		time.sleep(2)
		self.score_label.remove_from_parent()
		self.score = 0
		self.score_label = sc.LabelNode(str(self.score), ('Futura', 40), parent=self, position=(self.size.w/2, self.size.h - 40), z_position=1)


	#Generate and set up meteors: position, kind/looks, duration (time spent on screen, or speed), points awarded, what's inside,
	#	damage sustained thus far, and damage/hits required for explosion.
	#Updates: 
	#   (1) Stop scoring when the meteors are destroyed. Score when the contents are collected. 
	#   (2) Not all meteors contain stuff - only some. 
	#   (3) For efficiency, do not run anything unless a meteor is to be generated. 
	def new_meteor(self):
		#Add new meteors. Divide the frequency by 60 since that is how often the screen is updated (frames per second) 
		if random.uniform(0, 1) < meteorsPerSec / 60:
			m = random.choice(metype)
			#This determines the speed of each meteor. Set here to allow overwriting for items containing UFOs
			duration = random.randint(meteorDuration[0], meteorDuration[1]) 
			#This is the level of damage (laser hits) required to destroy the item
			explode = hits2Explode['Big'] if 'Big' in m else hits2Explode['Med']
			#Not all meteors include stuff - only the given proportion
			if random.uniform(0, 1) < meteorFillPct:	
				#Smaller meteors bring higher scores
				score = 1 if 'Big' in m else 2 if 'Med' in m else 3 if 'Small' in m else 4
				#What's inside meteors after explosion. Use the score to assign gold, silver or bronze
				ins = 'Gold' if score == 4 else 'Silver' if score == 3 else 'Bronze'
				#What's inside: coin or bolt or UFO? Use 15% bolt, 10% UFO, and the rest coin 
				rand = random.uniform(0, 1)
				if rand < 0.15:
					inside = 'spc:Bolt' + ins
				elif rand < 0.25:
					inside = random.choice(ufos)
					explode = 2
					duration = 20
				else:
					inside = 'plf:Item_Coin' + ins
			else:
				score = 0
				inside = 'nothing'
			new = {'node': sc.SpriteNode(m, position = [random.randint(0, ss[0]), ss[1] + 50], z_position = -1), 'texture': m, 
				'duration': duration, 'destroyed': False, 'score': score, 'inside': inside, 'damage': 0, 'explode': explode}
			self.items.append(new)
			node = self.items[-1]['node']
			self.add_child(node)
			#Rotate, move and remove
			node.run_action(A.rotate_by(random.choice([20, -20]), random.randint(4, 15)))
			node.run_action(A.sequence(A.move_to(random.randint(0, ss[0]), - 50, self.items[-1]['duration']), A.remove()))


	def touch_began(self, touch):
		#What happens when touching the screen? One could move the ship, etc. - but I settled on shooting the laser
		#x, y = touch.location
		#move_action = A.move_to(x, y, 0.5, sc.TIMING_SINODIAL)
		#print(str(x) + ' : ' + str(y))
		#self.ship.run_action(move_action)
		self.shoot_laser(self.laser_at)
	

	def shoot_laser(self, laser_at):
		#Don't allow more lasers than this on the screen
		if len(self.lasers) >= 12:
			return
		#print(str(self.ship.position))
		sp = self.ship.position
		#laser_at specifies the X position and also the number of lasers to shoot
		for a in laser_at:
			laser = sc.SpriteNode('spc:LaserBlue10', position=(sp.x + a, sp.y), z_position=-1, parent=self)
			laser.run_action(A.sequence(A.move_by(a * 3, ss[1] + 50, 1.3), A.remove()))
			self.lasers.append(laser)
		#qsound.play_effect('arcade:Laser_3')
	

	def ufo_laser(self, node, laser_at=[0]):
		#Divide the frequency by 60 since that is how often the screen is updated (frames per second) 
		if random.uniform(0, 1) < lasersPerSec_UFO / 60:
			sp = node.position
			#laser_at specifies the X position and also the number of lasers to shoot
			for a in laser_at:
				laser = sc.SpriteNode('spc:LaserGreen11', position=(sp.x + a, sp.y), z_position=-1, parent=self)
				laser.run_action(A.sequence(A.move_by(a * 3, -ss.y, 2.5), A.remove()))
				ul = {'node': laser, 'texture': 'ula'}
				self.items.append(ul)
	

	#Check for collisions between items and ship. If with a meteor, laser or UFO, destroy. Otherwise, award points or do other stuff
	def ship_collisions(self):
		for m in self.items:
			#Delete items that finished their animation
			if not m['node'].parent:
				self.items.remove(m)
				continue
			if self.ship.destroyed:
				break

			#Handle the collisions between ship and other objects
			if m['node'].frame.intersects(self.ship.frame):
				#A. Destroy the ship if hit by meteor, UFO or laser
				if any(x in m['texture'] for x in ['Meteor', 'UFO', 'ula']):
					self.ship.destroyed = True
					sound.play_effect('arcade:Explosion_4', 0.2)
					self.boom(self.ship.texture, self.ship.position, pieces=15)
					self.ship.remove_from_parent()
					self.run_action(A.sequence(A.wait(3), A.call(self.new_game)))
				#B. Collect coins and score
				elif 'Coin' in m['texture']:
					self.score += m['score']
					self.score_label.text = str(self.score)
					#m['node'].remove_from_parent()
					#self.items.remove(m)
					sound.play_effect('digital:PowerUp2')
				#C. Collect gold bolts and score. In addition destroy all meteors and collect 5 points for each
				elif 'BoltGold' in m['texture']:
					sound.play_effect('digital:SpaceTrash1')
					#Explode all meteors
					for n in self.items:
						if 'Meteor' in n['texture']:
							n['score'] = 5
							n['inside'] = 'plf:Item_CoinGold'
							self.destroy_meteor(n)
							#n['node'].remove_from_parent()
							#self.items.remove(n)
					break
				#D. Collect silver bolts and score. In addition destroy all meteors and collect 3 points for each
				elif 'BoltSilver' in m['texture']:
					sound.play_effect('digital:SpaceTrash1')
					#Explode all meteors
					for n in self.items:
						if 'Meteor' in n['texture']:
							n['score'] = 3
							n['inside'] = 'plf:Item_CoinSilver'
							self.destroy_meteor(n)
							#n['node'].remove_from_parent()
							#self.items.remove(n)
					break
				#E. If bronze bolt, then get an extra laser (up to 5 total) 
				elif 'BoltBronze' in m['texture'] and len(self.laser_at) < 5:
					if len(self.laser_at) == 1:
						self.laser_at = [-18, 18]
					elif len(self.laser_at) == 2:
						self.laser_at = [-25, 0, 25]
					elif len(self.laser_at) == 3:
						self.laser_at = [-40, -20, 20, 40]
					elif len(self.laser_at) == 4:
						self.laser_at = [-50, -25, 0, 25, 50]
					sound.play_effect('digital:PhaserUp7')
				#Well... after scoring, the item must be removed, to avoid scoring again
				self.items.remove(m)				
				m['node'].remove_from_parent()


	def laser_hits(self):
		for laser in self.lasers:
			# When a laser has finished its animation, it is automatically removed from the scene by its Action sequence. 
			# When that's the case, also remove it from the `lasers` list, so it isn't checked for collisions anymore
			if not laser.parent:
				self.lasers.remove(laser)
				continue
			for m in self.items:
				#If it is not Meteor or UFO, continue
				if all(x not in m['texture'] for x in ['Meteor', 'UFO']):
					continue
				if 'UFO' in m['texture']:
					self.ufo_laser(m['node'])
				#if m['destroyed']:
				#	continue
				if laser.position in m['node'].frame:
					m['damage'] += 1
					#sound.play_effect('arcade:Laser_6')
					#Every laser can hit just once
					self.lasers.remove(laser)
					if m['damage'] >= m['explode']:
						#Award points for destruction of UFO 
						if 'UFO' in m['texture']:
							self.score += destroy_UFO_pts
							self.score_label.text = str(self.score)
						self.destroy_meteor(m)
					#It is important to use break here to jump to next laser. Otherwise, the deleted laser - still being used by the loop - 
					# may hit another meteor, and the extra laser deletion (not there anymore) will throw an error. 
					break


	#Explode a given object (of known texture and position) into X pieces
	def boom(self, texture, position, pieces=5):
		#initext = node.texture
		#node.texture = sc.Texture('plf:Item_Star')
		for i in range(pieces):
			m = sc.SpriteNode(texture, parent=self)
			m.position = position + (random.uniform(-20, 20), random.uniform(-20, 20))
			angle = random.uniform(0, math.pi*2)
			dx, dy = math.cos(angle) * 80, math.sin(angle) * 80
			m.run_action(A.move_by(dx, dy, 0.6, sc.TIMING_EASE_OUT))
			m.run_action(A.sequence(A.scale_to(0, 0.6), A.remove()))
		#Remove the nodes that exploded
		#Well not anymore: because they now transform into stars
		#node.remove_from_parent()
	

	def destroy_meteor(self, meteor):
		#meteor['destroyed'] = True
		sound.play_effect('arcade:Explosion_1', 0.2)
		#Ha!! Copy reference vs value
		#mty = objc_util.copy().meteor['node']
		self.boom(meteor['texture'], meteor['node'].position)
		#Stop awarding points when meteor is destroyed. Instead, wait until the star is collected
		#self.score += meteor['score']
		#self.score_label.text = str(self.score)
		if meteor['score'] > 0 and 'UFO' not in meteor['texture']:
			meteor['node'].texture = sc.Texture(meteor['inside'])
			if 'UFO' in meteor['inside']:
				meteor['explode'] = hits2Explode['UFO']
				meteor['damage'] = 0
				meteor['score'] = destroy_UFO_pts
			meteor['texture'] = meteor['inside']
		else:
			meteor['node'].remove_from_parent()
			self.items.remove(meteor)

		
sc.run(MyGame(), show_fps=True)


'''
2019-11: This version uses a rather/too complicated way of animating 
the meteors. Instead of calculating the coordinates and moving them
like the ship, better use the scene run_action framework, which makes things 
simpler and better. 		
				
		#Update existing meteors and delete them when out of screen
		#Wow. Need to iterate backwards, to avoid index out of range errors. Caused by element deletion, followed by iterating to the next element anyway. 
		loopover = range(len(self.mets) - 1, -1, -1)
		for m in loopover:
			self.mets[m]['xy'][1] -= self.mets[m]['speed']
			self.mets[m]['node'].position = self.mets[m]['xy']
			if self.mets[m]['xy'][1] < -20:
				self.mets[m]['node'].remove_from_parent()
				print('node deleted at ' + str(datetime.datetime.now())[14:23])
				del self.mets[m]
				sound.play_effect('arcade:Hit_2')


		This method of generating new meteors - based on time since latest meteor - is too complicated. Better rely on update() being run 60 times per second. 
		
		#The latest meteor must be used here - to avoid inserting every loop
		#Also, switched to using time. Using the position was not good enough because of different speeds
		if len(self.mets) >= 1:
			proceed = time.time() - self.mets[-1]['time'] > self.mets[-1]['secs2next']
		else:
			proceed = True
		if proceed:
'''
