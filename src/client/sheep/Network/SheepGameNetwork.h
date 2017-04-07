#ifndef SHEEP_GAMENETWORK_H
#define SHEEP_GAMENETWORK_H

/* +================================+
 *  Name: GameNetwork
 *  Author: Johann Ostero (P4220933)
 *  Type: Singleton
 *  --
 *  This class works as a wrap around the Networking classes;
 *  Just like how the World handles the objects, the GameNetwork handles any incoming and outgoing
        UDP/TCP messages.
 *  It makes sure that the messages being sent and receieved from the Erlang server
       is properly handled.
 *  It makes message sending more precise and safer by having API methods to the outside code,
        that require specific parameters, that are then concatenated to be compatible with the server.
        It then decides which protocol to send with.
 *  The TCP/UDP received messages are being handled by running on their own receive threads
        that parse the server information and handles it appropriately.
 * +================================+
 */

#include <string>
#include <thread>
#include <mutex>
#include <SFML/Graphics.hpp>

namespace Sheep
{
    class Network;
    class NetTCP;
    class NetUDP;
    class GameNetwork
    {
        #define GAME_NETWORK Sheep::GameNetwork::GetInstance()
        public:
            /* +=== Singleton Construction ===+ */
            static void Create();
            static void Destroy();
            static GameNetwork& GetInstance() { return *INSTANCE; }

            bool Initialize(const std::string& serverIp, const std::string& serverPort, char msgSeperator);

            // +=== close server
            void Close();

            bool IsRegistered() const;

            /* +=== Server Messages ===+ */
            /* + For sending messages, thew client is required
                to provide the name of the sender. This is
                because the names are the key for the
                erlang server and the World class to look up the client*/

            // Register opens up two threads; one for UDP and one for TCP receiving
            // Register will freeze the program until it has received a TCP message back,
            //  then it opens up the threads
            bool Register(const std::string& userName);
            void Unregister(const std::string& userName);

                // Will need to be called after being registered
            void Join(const std::string& userName, const sf::Vector2f& pos, float angle);

            void UpdatePosition(const std::string& userName, const sf::Vector2f& pos);
            void UpdateRotation(const std::string& userName, float angle);

                // Only resets the object to be active
            void Respawn(const std::string& name);

            void ShootProjectile(const std::string& userName, const sf::Vector2f& pos, float angle);

        private:
            static GameNetwork* INSTANCE;

            char mMessageSeperator;

            NetTCP* mTCP = nullptr;
            NetUDP* mUDP = nullptr;

            std::thread* mThread_tcp;
            std::thread* mThread_udp;
            void thread_receive(Network& netRecv); // <-- thread method

            bool mIsRegistered = false;
            bool mIsJoining = false;

            GameNetwork() {}
            virtual ~GameNetwork();
    };
}


#endif // SHEEP_GAMENETWORK_H
