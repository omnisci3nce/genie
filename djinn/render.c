#include "render.h"
#include "glad/glad.h"
#include "GLFW/glfw3.h"

bool renderer_init(renderer* ren) {
    glfwInit();

    // glfw window creation
    GLFWwindow *window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT,
                                          "Genie Demo App", NULL, NULL);
    if (window == NULL) {
        printf("Failed to create GLFW window\n");
        glfwTerminate();
        return false;
    }
    ren->window = window;

    glfwMakeContextCurrent(ren->window);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);

    // glad: load all OpenGL function pointers
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
        // ERROR("Failed to initialise GLAD \n");
        return false;
    }

    glEnable(GL_DEPTH_TEST);

    u32 shader = shader_create_separate("shaders/rectangle.vert", "shaders/rectangle.frag");
    ren->rect_shader = shader;

    // set up VBO
    glGenBuffers(1, &ren->vbo);
    glGenVertexArrays(1, &ren->vao);

    glBindBuffer(GL_ARRAY_BUFFER, ren->vbo);
    glBindVertexArray(ren->vao);

    // setup vertex attributes
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(float), (void *)0); // position
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(float),
                          (void *)(2 * sizeof(float))); // color
    glEnableVertexAttribArray(1);

    return true;
}

u32 shader_create_separate(const char* vert_shader, const char* frag_shader) {
    //   TRACE("Load shaders at %s and %s", vert_shader, frag_shader);
    int success;
    char info_log[512];

    u32 vertex = glCreateShader(GL_VERTEX_SHADER);

    const char *vertex_shader_src = string_from_file(vert_shader);
    if (vertex_shader_src == NULL) {
        // ERROR("EXIT: couldnt load shader");
        exit(-1);
    }
    glShaderSource(vertex, 1, &vertex_shader_src, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(vertex, 512, NULL, info_log);
        printf("%s\n", info_log);
        // ERROR("EXIT: vertex shader compilation failed");
        exit(-1);
    }

    // fragment shader
    u32 fragment = glCreateShader(GL_FRAGMENT_SHADER);
    const char *fragment_shader_src = string_from_file(frag_shader);
    if (fragment_shader_src == NULL) {
        // ERROR("EXIT: couldnt load shader");
        exit(-1);
    }
    glShaderSource(fragment, 1, &fragment_shader_src, NULL);
    glCompileShader(fragment);
    glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(fragment, 512, NULL, info_log);
        printf("%s\n", info_log);
        // ERROR("EXIT: fragment shader compilation failed");
        exit(-1);
    }

    u32 shader_prog;
    shader_prog = glCreateProgram();

    glAttachShader(shader_prog, vertex);
    glAttachShader(shader_prog, fragment);
    glLinkProgram(shader_prog);
    glDeleteShader(vertex);
    glDeleteShader(fragment);
    free((char *)vertex_shader_src);
    free((char *)fragment_shader_src);

    return shader_prog;
}