#ifndef SHEEP_OBJECT_H
#define SHEEP_OBJECT_H

/* +================================+
 *  Name: Object
 *  Author: Johann Ostero (P4220933)
 *  Type: Interface
 *  --
 *  This is the base Object, working as an Interface.
 *  Any object that wants to gain access to this information will have to inherit from this interface
 *  This class allows for:
        - Custom Update method
        - Renderer
        - On Collision Enter/Exit
        - Collision Tag determination
        - (In)Active states
 *  It is a very abstract Object, to make sure that anyone who wants to inherit from it is not bound
        to anything specific, such as Health, speed, e.t.c.
 *
 *  The Transform control lies within the sfml sprite object
 *
 *  Every object to have a Name, Tag and a spriteId.
 *  For Collision checking, you are required to add collision tags.
 * +================================+
 */


#include <SFML/Graphics.hpp>
#include "SheepTag.h"

#include <unordered_map>

namespace Sheep
{
    class Object
    {
    public:
        Object(const std::string& objectName, Tag objectTag, int spriteId);
        virtual ~Object();

        /* +==== Copy ====+ */
        Object(const Object& cpy);
        Object& operator = (const Object& rhs);

        virtual void Update() = 0;
        virtual void Render();

        /* +=== Collision Checking ===+ */
        virtual void OnCollisionEnter(Object* otherObject) {}
        virtual void OnCollisionExit(Object* otherObject)  {}

        void CollisionCheck(const std::unordered_map<std::string, Object*>& mapObjects);

        /* +=== Tags Handling ====+ */
        void AddCollisionTag(Tag newTag);
        void SetCollisionTags(const std::vector<Tag>& tags);
        const std::vector<Tag>& GetCollisionTags() const;

        /* +=== Setters ===+ */
        void SetActive(bool state);
        void SetName(const std::string& name);

        /* +=== Getters ===+ */
        std::string GetName() const;
        bool IsActive() const;
        Tag GetTag() const;

        sf::Sprite* sprite = nullptr;
    private:
        std::string mName;
        bool mIsActive;

        std::vector<Tag> mCollideTags;
        Tag mTag;
        Object* mCurrentHitObject = nullptr;

        int mSprite_id;
    };
}



#endif // SHEEP_OBJECT_H
