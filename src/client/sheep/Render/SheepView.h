#ifndef SHEEP_VIEW_H
#define SHEEP_VIEW_H

/* +================================+
 *  Name: View
 *  Author: Johann Ostero (P4220933)
 *  Type: Singleton
 *  --
 *  This class is responsible for rendering everything on screen.
 *  It relies on the SFML window to handle the rendering.
 *  It uses shared texture access to draw textures more efficently.
 *
 *  The class allows for adding textures/fonts, that will return an id, that can be used
    when specifying what to draw.
 * +================================+
 */


#include <SFML/Graphics.hpp>

namespace Sheep
{
    #define VIEW Sheep::View::GetInstance()
    class View
    {
    public:
        /* +=== Singleton construction ===+ */
        static void Create();
        static void Destroy();
        static View& GetInstance() { return *INSTANCE; }

        bool Initialise(int screenWidth, int screenHeight, const std::string& windowTitle);

        /* +=== Textures ===+ */
            // Checks to see if it was possible to load a texture.
            // It will overwrite the textureId variable
        bool AddTexture(const std::string& filename, int& textureId);
        sf::Texture* GetTexture(int textureId);

            // Exact same as with the AddTexture
        bool AddFont(const std::string& filename, int& fontId);
        sf::Font* GetFont(int fontId);

        /* +=== Window handling  ===+ */
        sf::RenderWindow window;

    private:
        static View* INSTANCE;

        std::vector<sf::Texture*> mTextures;
        std::vector<sf::Font*> mFonts;

        View() {}
        virtual ~View();
    };
}

#endif // SHEEP_VIEW_H
