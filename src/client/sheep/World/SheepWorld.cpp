#include "SheepWorld.h"

#include "../Network/SheepGameNetwork.h"
#include "SheepView.h"

#include "SheepObject.h"
#include <assert.h>
#include <iostream>

using namespace Sheep;

/* +=== Singleton Construction ===+ */
World* World::INSTANCE = nullptr;

void World::Create()
{
    if(!INSTANCE)
        INSTANCE = new World();
}

void World::Destroy()
{
    if(INSTANCE)
    {
        delete INSTANCE;
        INSTANCE = nullptr;
    }
}

World::~World()
{
    for(auto& obj : mMapObjects)
    {
        delete obj.second;
        obj.second = nullptr;
    }
    mMapObjects.clear();

    delete mBackground;
    mBackground = nullptr;
}

bool World::Initialise(int windowWidth, int windowHeight, const std::string& windowTitle)
{
    VIEW.Create();
    if(!VIEW.Initialise(windowWidth, windowHeight, windowTitle))
        return false;

    mBackground = new sf::RectangleShape(sf::Vector2f(windowWidth,windowHeight));
    mBackground->setFillColor(sf::Color(0, 0, 0, 255));

    return true;
}

/* +=== Background ===+ */
void World::SetBackground(int textureId)
{
    mBackground->setTexture(VIEW.GetTexture(textureId));
    mBackground->setFillColor(sf::Color(255, 255, 255, 255));
}

/* +=== Object Handling ===+ */
void World::AddObject(Object* newObject)
{
    mMapObjects.insert(std::pair<std::string, Object*>(newObject->GetName(), newObject));

}

void World::AddProjectile(Object* newProjectile, unsigned int count)
{
    for(int i = 0; i < count; i++)
    {
        Object* newProj = newProjectile;
        mMapObjects.insert(std::pair<std::string, Object*>("bullet" + i, newProj));
    }
}

void World::ShootProjectile(const Object& shooter)
{
    for(auto& proj : mMapObjects)
    {
        // Iterates over every object, checking if they have the correct Tag and if they are inactive.
        //  Reactivating them and moving them into position
        if(proj.second->GetTag() == Tag::PROJECTILE && !proj.second->IsActive())
        {
            proj.second->SetCollisionTags(shooter.GetCollisionTags()); // <-- gives the projectile the tags to check collision with
            proj.second->sprite->setPosition(shooter.sprite->getPosition());
            proj.second->sprite->setRotation(shooter.sprite->getRotation());
            proj.second->SetActive(true);
            break;
        }
    }
}
void World::ShootProjectile(const std::string& shooterName, const sf::Vector2f& position, float angle)
{
    Object* obj = mMapObjects[shooterName];
    if(obj)
    {
        for(auto& proj : mMapObjects)
        {
            if(proj.second->GetTag() == Tag::PROJECTILE && !proj.second->IsActive())
            {
                proj.second->SetCollisionTags(obj->GetCollisionTags());
                proj.second->sprite->setPosition(position);
                proj.second->sprite->setRotation(angle);
                proj.second->SetActive(true);
                break;
            }
        }
    }
}

void World::RemoveObject(const std::string& name)
{
    assert(mMapObjects[name] != nullptr);

    delete mMapObjects[name];
    mMapObjects[name] = nullptr;

    mMapObjects.erase(name);
}

void World::UpdateObjectPosition(const std::string& name, const sf::Vector2f& pos)
{
    Object* obj= mMapObjects[name];
    if(obj != nullptr)
        obj->sprite->setPosition(pos);
}

void World::UpdateObjectRotation(const std::string& name, float angle)
{
   Object* obj= mMapObjects[name];
    if(obj != nullptr)
        obj->sprite->setRotation(angle);
}

void World::RespawnObject(const std::string& name)
{
    Object* obj = mMapObjects[name];
    if(obj != nullptr)
        obj->SetActive(true);
}

/* +=== Texture id handling === */
void World::LoadTexture(int id, const std::string& name)
{
    mTextureIds.insert(std::pair<std::string, int>(name, id));
}

/* +=== World Update ====+ */
void World::CheckEvents()
{
    // Event Handler
    sf::Event event;
    while (VIEW.window.pollEvent(event))
    {
        if (event.type == sf::Event::Closed)
            VIEW.window.close();
    }
}

void World::Run(unsigned int framerate)
{
    VIEW.window.setFramerateLimit(framerate);

    while(VIEW.window.isOpen())
    {
        VIEW.window.draw(*mBackground);
        CheckEvents();

        // Map Objects
        for(auto& obj : mMapObjects)
        {
            obj.second->Update();
            obj.second->Render();
            obj.second->CollisionCheck(mMapObjects);
        }

        VIEW.window.display();
    }
    VIEW.Destroy();
}
