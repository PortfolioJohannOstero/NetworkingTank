#ifndef SHEEPWORLD_H
#define SHEEPWORLD_H

/* +================================+
 *  Name: World
 *  Author: Johann Ostero (P4220933)
 *  Type: Singleton
 *  --
 *  This class works as the World of the program, tying everything together,
        When Run is called, the program will activate the game loop
 *  The World class stores every Object (that is provided) and will call it their Update() and Render() methods
        over every loop cycle.
 *  The object can be Added using AddObject or AddProjectile.
 *  It allows for MessageHandling, such as ShootProjectile
 *
 *  The World and server both use the name to gain access to the given client

 * +================================+
 */

#include <unordered_map>
#include <SFML/Graphics.hpp>

namespace Sheep
{
    class GameNetwork;
    class Object;
    class World
    {
        #define WORLD Sheep::World::GetInstance()
        public:
            /* +=== Singleton construction ====+ */
            void Create();
            void Destroy();
            static World& GetInstance() { return *INSTANCE; }

            bool Initialise(int windowWidth, int windowHeight, const std::string& windowTitle);

            /* +=== Background ===+ */
            void SetBackground(int textureId);

            /* +=== Object Handling ===+ */
            void AddObject(Object* newObject);
            void AddProjectile(Object* newProjectile, unsigned int count);

            void RemoveObject(const std::string& name);

            // Allows for updating objects externally
            void UpdateObjectPosition(const std::string& name, const sf::Vector2f& pos);
            void UpdateObjectRotation(const std::string& name, float angle);
            void RespawnObject(const std::string& name);


            /* +=== Message Handling ===+ */
            void ShootProjectile(const Object& shooter);
            void ShootProjectile(const std::string& shooterName, const sf::Vector2f& position, float angle);

            /* +=== Texture id handling === */
            void LoadTexture(int id, const std::string& name);

            /* +=== World Update ===+ */
            void CheckEvents();
            void Run(unsigned int framerate);

        private:
            static World* INSTANCE;

            std::unordered_map<std::string, Object*> mMapObjects;
            std::unordered_map<std::string, int> mTextureIds;

            sf::Shape* mBackground = nullptr;

            World() {}
            ~World();
    };
}

#endif // SHEEPWORLD_H
