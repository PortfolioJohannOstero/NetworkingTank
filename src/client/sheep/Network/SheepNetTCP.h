#ifndef SHEEPNETTCP_H
#define SHEEPNETTCP_H

/* +================================+
 *  Name: NetTCP
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class inherits' from Network.
 *  This class is responsible for everything to do with the TCP protocol on the client side
 *  NetTCP allows the user to connect to the server as well as send and receive messages.
 *  Using the Linux Socket Library
 * +================================+
 */

#include "SheepNetwork.h"

namespace Sheep
{
    class NetTCP : public Network
    {
        public:
            NetTCP();
            virtual ~NetTCP();

            // Setups up the socket to use TCP
            bool InitSocket() override final;

            bool Connect(const std::string& hostname, const std::string& serverPort);

            int Send(const std::string& message) override final;
            std::string Receive() override final;
    };
}

#endif // SHEEPNETTCP_H
