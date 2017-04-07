-record(r_udp, {address="localhost", port=2000}).
-record(r_tcp, {socket}).

-record(r_pos, {posX = 0, posY = 0}).

-record(r_client, {
				   name = "John Doe",
				   rotation = 0,
		 		   r_pos=#r_pos{},
				   r_tcp=#r_tcp{},
				   r_udp=#r_udp{}
				  }).
