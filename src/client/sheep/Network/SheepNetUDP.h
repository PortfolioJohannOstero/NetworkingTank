#ifndef SHEEPNETUDP_H
#define SHEEPNETUDP_H

/* +================================+
 *  Name: NetUDP
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class inherits' from Network.
 *  This class is responsible for everything to do with the UDP protocol on the client side
 *  NetUDP allows the user to open a socket, however still requires the servers IP and port for future usage
 *  This class also allows for sending and receiving messages.
 *  Using the Linux Socket Library
 * +================================+
 */

#include "SheepNetwork.h"

namespace Sheep
{
    class NetUDP : public Network
    {
        public:
            NetUDP();
            virtual ~NetUDP();

            bool InitSocket() override;

            virtual bool Open(const std::string& hostname, const std::string& serverPort);

            int Send(const std::string& message) override final;
            std::string Receive() override;

            uint16_t GetPort() const;
            std::string GetIP() const;

        private:
            struct sockaddr_in *mHostSocketAddress = nullptr;
            int mLocalPort = 0;

    };
}

#endif // SHEEPNETUDP_H
