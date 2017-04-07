+=====================+
	Client Controls
		-------

Arrow keys:
+Up/Down 	: forward/backwards
+Left/Right	: turn left/right

Space		: shoot

+=====================+

	Server Messages
		-------
UDP:
+	sendtoall:name:message
Sends a message to every one on the server, providing the name of the sender

+	sendto:senderName:receiverName:message
Sends a message to a specific person on the server

+	set_pos:name:posX:posY
updates the client's (with the given name) position
	-Sends:	"update_pos"

+	set_rot:name:angle
updates the client's (with the given name) rotation
	-Sends:	"update_rot"

+	shoot:name:posX:posY:angle
tells everyone that the client is shooting a projectile with the initial
position and rotation

--
TCP:
+	respawn:name
tells everyone on the server to reactivate the client on the clientside
	-Sends:	"respawned:name"

+	reg:name:ip:port:posX:posY
registers the client to the message router

+	unreg:name
removes the client from the message router
	-Sends:	"removed_client"

+	join:name:posX:posY:angle
tells everyone on the server to create a new object with the given position and rotation (and name)
	-Sends:	"join_client:name:posX:posY:angle"

+	sendtoall:name:message
Sends a message to every one on the server, providing the name of the sender

+	sendto:senderName:receiverName:message
Sends a message to a specific person on the server
+===================+

