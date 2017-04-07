#include "SheepObject.h"

#include "SheepView.h"

#include <vector>

using namespace Sheep;

Object::Object(const std::string& objectName, Tag objectTag, int spriteId)
    :mName(objectName), mTag(objectTag)
{
    // gets the texture from the View class
    sprite = new sf::Sprite(*VIEW.GetTexture(spriteId));

    // sets the origin to be the center of the object
    sf::FloatRect spriteBox = sprite->getLocalBounds();
    sprite->setOrigin(spriteBox.width/2, spriteBox.height/2);
}


Object::~Object()
{
    delete sprite;
    sprite = nullptr;

    mCollideTags.clear();
}

/* +==== Copy ====+ */
Object::Object(const Object& cpy)
{
    *this = cpy;
}

Object& Object::operator = (const Object& rhs)
{
    *sprite = *rhs.sprite;
    mName = rhs.mName;
    mIsActive = rhs.mIsActive;
    mSprite_id = rhs.mSprite_id;

    mTag = rhs.mTag;

    mCollideTags.clear();
    mCollideTags.resize(rhs.mCollideTags.size());
    for(unsigned int i = 0; i < mCollideTags.size(); i++)
        mCollideTags[i] = rhs.mCollideTags[i];

    return *this;
}

void Object::Render()
{
    if(mIsActive) // <-- Only renders if the object is active
    {
        VIEW.window.draw(*sprite);
    }
}

// This method is required to run for checking if it is colliding with anything
void Object::CollisionCheck(const std::unordered_map<std::string, Object*>& mapObjects)
{
    // When writing "THIS object" it is refered to the local object
    // "the object" is refered to the currently iterated object

    if(mCollideTags.size() <= 0 || !mIsActive)
        return;

    // Iterates over ever object in the world
    for(auto& obj : mapObjects)
    {
        // If THIS object is the same as the one being checked against, then just continue
        // ignore if the object is inactive
        if(obj.second == this || !obj.second->IsActive())
            continue;

        // Looks to see if THIS object has the object Tag in its collisionTag container
        bool found = false;
        for(auto colTag : mCollideTags)
        {
            if(colTag == obj.second->mTag)
            {
                found = true;
                break;
            }
        }

        if(!found)
            continue;

        // Checks for a collision
        if(obj.second->sprite->getGlobalBounds().intersects(sprite->getGlobalBounds()))
        {
            // if collided, then call the childs OnCollisonEnter (if overwritten)
            this->OnCollisionEnter(obj.second);
            mCurrentHitObject = obj.second;
        }
        else if(mCurrentHitObject != nullptr)
        {
            // if the object has already collided, this is called when they are not colliding anymore
            this->OnCollisionExit(obj.second);
            mCurrentHitObject = nullptr;
        }
    }

}

/* +=== Setters ===+ */
void Object::SetActive(bool state)
{
    mIsActive = state;
}

void Object::SetName(const std::string& name)
{
    mName = name;
}

/* +=== Tags Handling ====+ */
void Object::AddCollisionTag(Tag newTag)
{
    mCollideTags.push_back(newTag);
}

// Sets the entire collision tag container directly
void Object::SetCollisionTags(const std::vector<Tag>& tags)
{
    mCollideTags = tags;
}

const std::vector<Tag>& Object::GetCollisionTags() const
{
    return mCollideTags;
}

/* +=== Getters ===+ */
std::string Object::GetName() const
{
    return mName;
}

bool Object::IsActive() const
{
    return mIsActive;
}

Tag Object::GetTag() const
{
    return mTag;
}
