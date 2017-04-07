#include "SheepView.h"

#include <assert.h>

using namespace Sheep;

/* +==== Singleton Construction ====+ */
View* View::INSTANCE = nullptr;

void View::Create()
{
    if(!INSTANCE)
        INSTANCE = new View();
}

void View::Destroy()
{
    if(INSTANCE)
    {
        delete INSTANCE;
        INSTANCE = nullptr;
    }
}

/* +==== Destructor ====+ */
View::~View()
{
    if(window.isOpen())
        window.close();

    for(sf::Texture* texture : mTextures)
    {
        delete texture;
        texture = nullptr;
    }

    for(sf::Font* font : mFonts)
    {
        delete font;
        font = nullptr;
    }
}

/* +====  Initialising the view ====+ */
bool View::Initialise(int screenWidth, int screenHeight, const std::string& windowTitle)
{
    window.create(sf::VideoMode(screenWidth, screenHeight), windowTitle);
    return true;
}

bool View::AddTexture(const std::string& filename, int& textureId)
{
    sf::Texture* newTexture = new sf::Texture();
    if(!newTexture->loadFromFile(filename))
        textureId = -1;
    else
    {
        textureId = mTextures.size();
        mTextures.push_back(newTexture);
    }

    return textureId >= 0;
}

sf::Texture* View::GetTexture(int textureId)
{
    assert(textureId > -1);
    assert(textureId < mTextures.size());

    return mTextures[textureId];
}

bool View::AddFont(const std::string& filename, int& fontId)
{
    sf::Font* newFont = new sf::Font();
    if(!newFont->loadFromFile(filename))
        fontId = -1;
    else
    {
        fontId = mFonts.size();
        mFonts.push_back(newFont);
    }

    return fontId >= 0;
}

sf::Font* View::GetFont(int fontId)
{
    assert(fontId > -1);
    assert(fontId < mFonts.size());

    return mFonts[fontId];
}
