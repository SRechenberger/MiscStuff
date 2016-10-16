#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/ip.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <wait.h>

#define UERR 1
#define SERR 2
#define ISFILE 1
#define ISPATH 2

char* cmd;

void user_err(const char* msg){
  fprintf(stderr,
    "User error: %s\n"
    "Usage:\n"
    "  %s <PORT> [ --path <PATH> | --file <FILE> ] <ARG>*\n"
    "    PORT: port number\n"
    "    PATH: absolute path to the executeable\n"
    "    FILE: executable or command\n"
    "    ARG:  Command line argument\n",
    msg, cmd);
}

void s_err(const char* msg, int errnum){
  fprintf(stderr,
    "System error: %s\n"
    "System message: %s\n",
    msg, strerror(errnum));
}

void process_err(pid_t pid, int stat){
  if(WIFEXITED(stat)){
    fprintf(stderr,
        "Process %d terminated with exit code %d\n",
        pid, WEXITSTATUS(stat));
  } else if(WIFSIGNALED(stat)){
    fprintf(stderr,
        "Process %d signaled by signal %d\n",
        pid, WTERMSIG(stat));
  } else if(WIFSTOPPED(stat)){
    fprintf(stderr,
        "Process %d stopped by signal %d\n",
        pid, WSTOPSIG(stat));
  } else {
    fprintf(stderr,
        "Something unexpected happend to process %d\n",
        pid);
  }
}

void try_close(int fd){
  if(close(fd) < 0){
    s_err("close", errno);
    // exit(SERR);
  }
}

void wait_for_all(){
  int stat;
  pid_t pid;
  while((pid = wait(&stat)) > 0){
    process_err(pid, stat);
  }
}

int main(int argc, char* args[]){
  cmd = (args++)[0]; argc--;

  /** Parse arguments **/
  int port = -1;
  char *executable = 0;
  int file_or_path = -1;
  char **arguments = 0;
  if(argc < 3){
    user_err("not enough arguments");
    exit(UERR);
  }
  port = atoi(args[0]);
  if(!port){
    user_err("no valid port number");
    exit(UERR);
  }
  if(strcmp("--path", args[1]) == 0){
    executable = args[2];
    file_or_path = ISPATH;
  } else if(strcmp("--file", args[1]) == 0){
    executable = args[2];
    file_or_path = ISFILE;
  } else {
    user_err("no executable");
    exit(UERR);
  }
  arguments = &args[2];

  /** Init TCP connection */
  int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
  if(sock_fd < 0){
    s_err("socket", errno);
    exit(SERR);
  }
  int optval = 1;
  if(setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) < 0){
    s_err("setsockopt", errno);
    try_close(sock_fd);
    exit(SERR);
  }

  struct sockaddr_in addr = {0};
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(port);
  if(bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr)) < 0){
    s_err("bind", errno);
    try_close(sock_fd);
    exit(SERR);
  }

  if(listen(sock_fd, SOMAXCONN) < 0){
    s_err("listen", errno);
    try_close(sock_fd);
    exit(SERR);
  }
  /** TCP init done **/

  /** Accept main loop */
  int conn_fd;
  while((conn_fd = accept(sock_fd, 0, 0)) > 0){
    /* Fork process */
    pid_t pid = fork();
    if(pid < 0){
      s_err("fork", errno);
      try_close(conn_fd);
      try_close(sock_fd);
      exit(SERR);
    } else if(pid == 0){
      if(dup2(conn_fd, 0) < 0){
        s_err("dup2", errno);
        exit(SERR);
      }
      if(dup2(conn_fd, 1) < 0){
        s_err("dup2", errno);
        exit(SERR);
      }
      if(file_or_path == ISFILE){
        execvp(executable, arguments);
        s_err("execvp", errno);
        exit(SERR);
      } else if(file_or_path == ISPATH){
        execv(executable, arguments);
        s_err("execv", errno);
        exit(SERR);
      } else {
        user_err("unexpected: 'file_or_path' unset");
        exit(UERR);
      }
    } else {
    /* Close fd */
      if(close(conn_fd) < 0){
        s_err("close(conn_fd)", errno);
        try_close(sock_fd);
        wait_for_all();
        exit(SERR);
      }
    }
  }
  /** Quit */
  wait_for_all();
  try_close(sock_fd);
  exit(0);

}
