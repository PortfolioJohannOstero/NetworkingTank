#ifndef SHEEPNETUDPBROADCAST_H
#define SHEEPNETUDPBROADCAST_H

/* +================================+
 *  Name: NetUDPBroadcast
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class inherits' from NetUDP.
 *  This class operates just like NetUDP class, only it does not send messages.
 *  This class is designed to only accept messages coming from the broadcasted IP.
        When receiving, it will return a message but also obtain the senders IP address
        and store it for later use.
 * +================================+
 */

#include "SheepNetUDP.h"

namespace Sheep
{
    class NetUDPBroadcast : public NetUDP
    {
        public:
            NetUDPBroadcast();
            virtual ~NetUDPBroadcast();

            // Requires the broadcast ip and the broadcast port
            bool Open(const std::string& hostname, const std::string& serverPort) override final;

            bool InitSocket() override final;

            // returns a message and stores the senders IP address in mServerIP
            std::string Receive() override final;

            std::string GetServerIP() const;

        private:
            std::string mServerIP;

    };
}


#endif // SHEEPNETUDPBROADCAST_H
