#ifndef SHEEPNETWORK_H
#define SHEEPNETWORK_H

/* +================================+
 *  Name: Network
 *  Author: Johann Ostero (P4220933)
 *  Type: Interface
 *  --
 *  This class works as in interface for both TCP and UDP.
 *  It also allows for common method acess, that both of them will need but do not need to modify.
 *  This class determines the message buffer size for the receive methods and holds the socket
 *
 *  The Network interface also allows for easy access to setting up the address
 * +================================+
 */


#include <string>

#include <arpa/inet.h>
#include <netdb.h>

#ifdef __linux__
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#elif __APPLE__
#include <unistd.h>
#endif

#include "SheepMessageBuffer.h"

#define BUFFER_SIZE 1024


namespace Sheep
{
    class Network
    {
        public:
            Network();
            virtual ~Network();

            virtual bool InitSocket() = 0;
            virtual void CloseSocket();

            virtual std::string Receive() = 0;
            virtual int Send(const std::string& message) = 0;

        protected:
            // Converts the stringed ip and port into a sockaddr_in*
                // ___socket_type is the SOCK_DGRAM / SOCK_STREAM
            struct sockaddr_in* SetupAddress(const std::string& ip,
                                             const std::string& port,
                                             __socket_type protocolType);

          int mSocket = -1;
          MessageBuffer* mRecvBuffer = nullptr;
    };

}


#endif // SHEEPNETWORK_H
