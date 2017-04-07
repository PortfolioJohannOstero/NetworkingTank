#include "SheepObjectEnemy.h"
#include "SheepView.h"

using namespace Sheep;

ObjectEnemy::ObjectEnemy(float speed, const std::string& objectName, int spriteId, Tag objectTag, int fontId, float labelOffset)
    : Object(objectName, objectTag, spriteId), mSpeed(speed), mLabelOffset(labelOffset)
{
    mPrevPos = sprite->getPosition();

    // Setup the label
    mNameLabel.setString(GetName());
    mNameLabel.setCharacterSize(18);
    mNameLabel.setFont(*VIEW.GetFont(fontId)); // <-- Gets the font from the View class
    mNameLabel.setColor(sf::Color::White);
}

ObjectEnemy::~ObjectEnemy()
{

}

void ObjectEnemy::Update()
{
    if(IsActive())
    {
        sf::Vector2f currPos = sprite->getPosition();
        mNameLabel.setPosition(sf::Vector2f(currPos.x - GetName().length() - 18, currPos.y - mLabelOffset));
    }
}

void ObjectEnemy::Render()
{
    if(IsActive())
    {
        VIEW.window.draw(*sprite);
        VIEW.window.draw(mNameLabel);
    }
}
