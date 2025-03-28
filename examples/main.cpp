//
// Created by Min Zhou on 2021/04/09.
//

#include <lua.hpp>
#include <string>

int loadFile(lua_State *L, const std::string &fileName)
{
    auto err = luaL_loadfile(L, fileName.c_str());
    if (err)
    {
        printf("error load file %s. error[%s]\n", fileName.c_str(), lua_tostring(L, -1));
        return err;
    }
    err = lua_pcall(L, 0, 0, 0);
    if (err)
    {
        printf("error load file %s, when call lua_pcall. error[%s]\n", fileName.c_str(), lua_tostring(L, -1));
        lua_pop(L, 1);
    }
    return err;
}

int main()
{
    lua_State *L = luaL_newstate();
    luaL_openlibs(L);
    for (int i = 0; i < 5; ++i)
    {
        int pre = lua_gettop(L);
        loadFile(L, "test.lua");
        int after = lua_gettop(L);
        printf("from %d to %d\n", pre, after);
    }

    return 0;
}
