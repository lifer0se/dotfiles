#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* concat(const char *s1, const char *s2)
{
  char *result = malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

int main(int argc, char** argv) {
  if (argc <= 1)
    return 0;

  FILE *pp;
  pp = popen("wmctrl -d", "r");
  if (pp != NULL) {
    while (1) {
      char *line;
      char buf[1000];
      line = fgets(buf, sizeof buf, pp);
      if (line == NULL) break;

      char *res;
      res = strstr(line, "*");
      if (res) {
        char *c = &line[0];
        int num = atoi(c);
        int dir;
        if (strcmp(argv[1], "r") == 0)
          dir = 1;
        else if (strcmp(argv[1], "l") == 0)
          dir = -1;

        if ((num > 0 && num < 8) ||
            (num > 9 && num < 17) ||
            (num == 0 && dir > 0) ||
            (num == 9 && dir > 0) ||
            (num == 8 && dir < 0) ||
            (num == 17 && dir < 0))
          num += dir;
        else if (num == 0)
          num = 8;
        else if (num == 8)
          num = 0;
        else if (num == 9)
          num = 17;
        else if (num == 17)
          num = 9;

        char str[2];
        sprintf(str, "%d", num);
        system(concat("wmctrl -s ", str));
        break;
      }
    }
    pclose(pp);
  }

  return 0;
}
