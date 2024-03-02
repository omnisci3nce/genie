#include "render.h"
#include "glad/glad.h"
#include "GLFW/glfw3.h"

#include "djinn.h" // Might wanna move things around so I'm not importing this directly while render.h
                   // is importing it.

#include <unistd.h>

bool renderer_init(renderer *ren)
{
    glfwInit();

    // glfw window creation
    GLFWwindow *window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT,
                                          "Genie Demo App", NULL, NULL);
    if (window == NULL)
    {
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
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        // ERROR("Failed to initialise GLAD \n");
        return false;
    }

    glEnable(GL_DEPTH_TEST);

    printf("Compiling shaders\n");
    u32 shader = shader_create_separate("djinn/shaders/rectangle.vert", "djinn/shaders/rectangle.frag");
    printf("finished\n");
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

    ren->draw_cmd_buf = draw_rect_darray_new(10);

    return true;
}

typedef struct params
{
    int x, y, width, height;
    float r, g, b;
} params;

void draw_rectangle(params *params)
{
    draw_rect cmd = {
        .x = params->x,
        .y = params->y,
        .width = params->width,
        .height = params->height,
        .r = params->r,
        .g = params->g,
        .b = params->b,
    };
    draw_rect_darray_push(g_djinn.render.draw_cmd_buf, cmd);
}

DECL_TYPED_ARRAY(f32)

void ui_draw() {
    renderer ren = g_djinn.render;
    // lets just be dumb and allocate a fresh darray per frame and clean it up at the end
    f32_darray *vertices = f32_darray_new(ren.draw_cmd_buf->len);

    // INFO("Draw calls %d", state->draw_cmd_buf->len);
    draw_rect_darray_iter iter = draw_rect_darray_iter_new(ren.draw_cmd_buf);
    draw_rect* cmd;
    while ((cmd = draw_rect_darray_iter_next(&iter)))
    {
            i32 x = cmd->x;
            i32 y = cmd->y;
            u32 width = cmd->width;
            u32 height = cmd->height;
            struct ui_bound viewport_x = {
                .min = 0., .mid = SCR_WIDTH / 2.0, .max = SCR_WIDTH, .ext = 0};
            struct ui_bound viewport_y = {
                .min = 0., .mid = SCR_HEIGHT / 2.0, .max = SCR_HEIGHT, .ext = 0};
            struct ui_bound ndc = {.min = -1, .mid = 0, .max = 1, .ext = 0};
            vec2f v0 = {scale_pt((float)x, viewport_x, ndc), -scale_pt((float)y, viewport_y, ndc)};
            vec2f v1 = {scale_pt((float)(x + width), viewport_x, ndc),
                       -scale_pt((float)y, viewport_y, ndc)};
            vec2f v2 = {scale_pt((float)(x + width), viewport_x, ndc),
                       -scale_pt((float)(y + height), viewport_y, ndc)};
            vec2f v3 = {scale_pt((float)x, viewport_x, ndc),
                       -scale_pt((float)(y + height), viewport_y, ndc)};
            // NOTE: I think I can get rid of the scaling and just pass in a orthographic perspective
            //       matrix instead and do it all on GPU
            vec2f *verts[4] = {&v0, &v1, &v2, &v3};
            size_t indices[] = {0, 2, 3, 0, 1, 2};
            for (size_t v = 0; v < 6; v++)
            {
                vec2f *vert = verts[indices[v]];
                float vx = (*vert).x;
                float vy = (*vert).y;
                f32_darray_push(vertices, vx);
                f32_darray_push(vertices, vy);
                f32_darray_push(vertices, cmd->r);
                f32_darray_push(vertices, cmd->g);
                f32_darray_push(vertices, cmd->b);
                // printf("x %f y %f \n", vx, vy);
                // printf("r %f g %f b %f\n", cmd->rect.colour.r, cmd->rect.colour.g, cmd->rect.colour.b);
            }
    }

    glUseProgram(ren.rect_shader);

    glBindVertexArray(ren.vao);
    glBindBuffer(GL_ARRAY_BUFFER, ren.vbo);
    size_t size = sizeof(f32) * vertices->len;
    glBufferData(GL_ARRAY_BUFFER, size, vertices->data, GL_STATIC_DRAW);
    glDrawArrays(GL_TRIANGLES, 0, vertices->len / 5);

    // reset buffers
    draw_rect_darray_clear(ren.draw_cmd_buf);
    f32_darray_free(vertices);
}

struct dummy;
void frame_begin(struct dummy *d)
{
    glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glClearColor(BACKGROUND.x, BACKGROUND.y, BACKGROUND.z, 1.0f);
}
void frame_end(struct dummy *d)
{
    ui_draw();
    glfwSwapBuffers(g_djinn.render.window);
    glfwPollEvents();
    usleep(16 * 1000);
}
bool window_should_close(struct dummy *d)
{
    return !glfwWindowShouldClose(g_djinn.render.window); // confusing but bools are flipped I think
}

u32 shader_create_separate(const char *vert_shader, const char *frag_shader)
{
    //   TRACE("Load shaders at %s and %s", vert_shader, frag_shader);
    int success;
    char info_log[512];

    u32 vertex = glCreateShader(GL_VERTEX_SHADER);

    const char *vertex_shader_src = string_from_file(vert_shader);
    if (vertex_shader_src == NULL)
    {
        // ERROR("EXIT: couldnt load shader");
        exit(-1);
    }
    glShaderSource(vertex, 1, &vertex_shader_src, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(vertex, 512, NULL, info_log);
        printf("%s\n", info_log);
        // ERROR("EXIT: vertex shader compilation failed");
        exit(-1);
    }

    // fragment shader
    u32 fragment = glCreateShader(GL_FRAGMENT_SHADER);
    const char *fragment_shader_src = string_from_file(frag_shader);
    if (fragment_shader_src == NULL)
    {
        // ERROR("EXIT: couldnt load shader");
        exit(-1);
    }
    glShaderSource(fragment, 1, &fragment_shader_src, NULL);
    glCompileShader(fragment);
    glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
    if (!success)
    {
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