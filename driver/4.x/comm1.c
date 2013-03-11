#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#if defined(_SEQUENT_)
#include <sys/procstats.h>
#endif
#include <fcntl.h>
#include <stdarg.h>
#include <arpa/telnet.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <memory.h>
#include <fcntl.h>
#include <stdlib.h>
#include "config.h"
#include "lint.h"
#include "mstring.h"
#include "interpret.h"
#include "comm.h"
#include "object.h"
#include "sent.h"
#include "simulate.h"
#include "super_snoop.h"
#include "ndesc.h"
#include "hname.h"
#include "nqueue.h"
#include "telnet.h"
#include "tcpsvc.h"
#include "udpsvc.h"

void set_prompt (char *);
void print_prompt (void);
void prepare_ipc (void);
void ipc_remove (void);
char *query_ip_number (struct object *);
char *query_host_name (void);
void add_ip_entry (unsigned long, char *);

extern char *string_copy(char *);
extern int d_flag;
extern int s_flag;
extern int current_time;
extern int eval_cost;
extern struct svalue *sp;
extern int no_ip_demon;

void remove_interactive(struct interactive *, int), add_ref(struct object *, char *);

extern void debug_message(char *, ...), free_sentence(struct sentence *);

struct interactive *all_players[MAX_PLAYERS];

extern int errno;
extern struct object *current_interactive;
extern struct object *previous_ob;
#ifdef CATCH_UDP_PORT
extern int udp_port;
#endif /* CATCH_UDP_PORT */

void *new_player(void *, struct sockaddr_in *, size_t);

int num_player;

/*
 * Interprocess communication interface to the backend.
 */

extern int port_number;
#ifdef PORTNO_SEC
extern int port_number_sec;
#endif

#ifdef COMM_STAT
int add_message_calls=0;
int inet_packets=0;
int inet_volume=0;
#endif

void 
prepare_ipc() 
{
    nd_init();
#ifndef NO_IP_DEMON
    if( !no_ip_demon ) 
      hname_init();
#endif
    telnet_init((u_short)port_number);
#ifdef PORTNO_SEC
    telnet_init((u_short)port_number_sec);
#endif
#ifdef SERVICE_PORT
    tcpsvc_init();
#endif /* SERVICE_PORT */
#ifdef CATCH_UDP_PORT
    udpsvc_init(udp_port);
#endif /* CATCH_UDP_PORT */

    (void)signal(SIGPIPE, SIG_IGN);
}

/*
 * This one is called when shutting down the MUD.
 */
void 
ipc_remove() 
{
    (void)printf("Shutting down ipc...\n");
    nd_shutdown();
}
void write_socket(char *, struct object *);

/*
 * Send a message to a player. If that player is shadowed, special
 * care has to be taken.
 */
/*VARARGS1*/

void
add_message2(struct object *ob, char *fmt, ...)
{
    char buff[10000];
    va_list argp;
    va_start(argp, fmt);
    (void)vsnprintf(buff, sizeof(buff), fmt, argp);
    /* LINTED: expression has null effect */
    va_end(argp);
    if (ob && !(ob->flags & O_DESTRUCTED))
        {
            push_string(buff, STRING_MSTRING);
            (void)apply("catch_tell", ob, 1, 1);
        }
    else
        write_socket(buff, (struct object *)0);
    
}

void
add_message(char *fmt, ...)
{
    char buff[10000];
    va_list argp;
    va_start(argp, fmt);
    (void)vsnprintf(buff, sizeof(buff), fmt, argp);
    /* LINTED: expression has null effect */
    va_end(argp);
    if (command_giver && !(command_giver->flags & O_DESTRUCTED))
    {
        push_string(buff, STRING_MSTRING);
        (void)apply("catch_tell", command_giver, 1, 1);
    }
    else
        write_socket(buff, (struct object *)0);
}

#ifdef COLOUR_SUPPORT

static colour_blocked = 0;

typedef enum colour_state{ 
  e_no_colour = 0x00,
  e_colour_cleared = 0x01,
  e_colour_code = 0x02
} colour_state;
 
typedef int(*colour_parser_func)( char *buf, char bg, char fg, int hilite );

/* Static data passed into the colour parsers */
struct colour_data {
  char *base_buffer;
  char default_fg;
  char default_bg;
};

static int
null_colour_parser( char *buf, char bg, char fg, int hilite ) {
  return 0;
}

static colour_state
colour( struct colour_data *data, char **toBuf, char **fromBuf, int realLength ,
        colour_parser_func colour_parser ) {
  char *bp;
  char *cp;
  colour_state last_code = e_no_colour;

  if( fromBuf == NULL ) {
    colour_blocked = 0;
    *toBuf += colour_parser( *toBuf, data->default_bg, data->default_fg, 0 );
    return e_colour_cleared;
  }

  bp = *toBuf;
  cp = *fromBuf;
  do {
    if (bp >= &(data->base_buffer[MAX_WRITE_SOCKET_SIZE] ) ) {
	error("Too long message!\n");
    }
    if( *cp == '\n' || *cp == '\0' ) break;
    if( !colour_blocked && *cp == '^' ) {
      switch( *(++cp) ) {
        case 'n': case 'N': case '*': 
          bp += colour_parser( bp, data->default_bg, data->default_fg, 0 );
          cp++;
          last_code = e_colour_cleared;
	  break;
        case '@': {
          char fg_char, bg_char;
          int hilite;

          /* Get the background code information */
          bg_char = *(++cp);
          if( *cp == '\n' || *cp == '\0' ) break;
          hilite = isupper( *cp ) ? 1 : 0;

          /* Get the foreground code information */
          fg_char = *(++cp);
          if( *cp == '\n' || *cp == '\0' ) break;
  
          /* Set fg/bg colours */
          bp += colour_parser( bp, fg_char, bg_char, hilite );
          last_code = e_colour_code;
          cp++;
          break;
        }
        case '<':
          colour_blocked = 1;
          last_code = e_colour_code;
          cp++;
          break;
        case '+': {
          char fg_char = *(++cp);
          if( *cp == '\n' || *cp == '\0' ) break;
          bp += colour_parser( bp, (char)0, fg_char, isupper( *cp ) ? 1 : 0 );
          last_code = e_colour_code;
          cp++;
          break;
        }
	case '-': {
          char bg_char = *(++cp);
          if( *cp == '\n' || *cp == '\0' ) break;
          bp += colour_parser( bp, bg_char, (char)0, isupper( *cp ) ? 1 : 0 );
          last_code = e_colour_code;
          cp++;
	  break;
	}
        case 'b': case 'B' : {
          bp += colour_parser( bp, (char)0, (char)0, isupper( *cp ) ? 1 : 0 );
	  last_code = e_colour_code;
          cp++;
          break;
        }
        case '\n': case '\0' : *bp++ = '^'; break;
	default:
          if( *cp != '^' ) *bp++ = '^';
	  *bp++ = *cp++;
          realLength--;
      }
    } else {
      *bp++ = *cp++;
      realLength--;
    }
  } while( realLength > 0 );

  *toBuf = bp;
  *fromBuf = cp;
  return last_code;
}

static int 
ansi_lookup( char x ) {
  switch( x ) {
    case 'l': case 'L': return  30; break;
    case 'r': case 'R': return  31; break;
    case 'g': case 'G': return  32; break;
    case 'y': case 'Y': return  33; break;
    case 'b': case 'B': return  34; break;
    case 'm': case 'M': return  35; break;
    case 'c': case 'C': return  36; break;
    case 'w': case 'W': return  37; break;
    case 'u': case 'U': return  4; break;
    case 'x': case 'X': return  5; break;
  }
  return 0;
}

int ansi_colour_parser( char *buf, char bg, char fg, int hilite ) {
  int bg_code = ansi_lookup( bg );
  int fg_code = ansi_lookup( fg );

  if( colour_blocked ) {
    return 0;
  } else if( bg_code && fg_code ) {
    return sprintf( buf, "\033[%1d;%d;%dm", hilite, bg_code + 10, fg_code );
  } else if( fg_code || bg_code ) {
    return sprintf( buf, "\033[%1d;%dm", hilite,
                    ( bg_code ) ? ( bg_code + 10 ) : fg_code );
  } else if( !bg && !fg ) {
    return sprintf( buf, "\033[%1dm", hilite );
  } else {
    return sprintf( buf, "\033[0m" );
  }
}

static char* 
debug_lookup( char x ) {
  switch( x ) {
    case 'l': case 'L': return  "BLACK"; break;
    case 'r': case 'R': return  "RED"; break;
    case 'g': case 'G': return  "GREEN"; break;
    case 'y': case 'Y': return  "YELLOW"; break;
    case 'b': case 'B': return  "BLUE"; break;
    case 'm': case 'M': return  "MAGENTA"; break;
    case 'c': case 'C': return  "CYAN"; break;
    case 'w': case 'W': return  "WHITE"; break;
    case 'u': case 'U': return  "UNDERLINE"; break;
    case 'x': case 'X': return  "BLINK"; break;
  }
  return NULL;
}

int debug_colour_parser( char *buf, char bg, char fg, int hilite ) {
  char *bg_str = debug_lookup( bg );
  char *fg_str = debug_lookup( fg );

  if( colour_blocked ) {
    return 0;
  } else if( bg_str && fg_str ) {
    return sprintf( buf, "[BG%s%s-FG%s]", (hilite?"HI":""), bg_str, fg_str );
  } else if( bg_str ) {
    return sprintf( buf, "[BG%s%s]", (hilite?"HI":""), bg_str );
  } else if( fg_str ) {
    return sprintf( buf, "[FG%s%s]", (hilite?"HI":""), fg_str );
  } else if( !bg && !fg ) {
    return sprintf( buf, "[BOLD-%s]", (hilite?"ON":"OFF") );
  }else {
    return sprintf( buf, "[RESET]" );
  }
}

static colour_parser_func colour_parsers[] = {
  null_colour_parser,
  ansi_colour_parser,
  debug_colour_parser
};

#endif

void
write_socket(char *cp, struct object *ob)
{
    struct interactive *ip;
#ifdef WORD_WRAP
    int len, current_column;
#endif

#if defined( WORD_WRAP ) || defined( COLOUR_SUPPORT )
    /** Size adjusted for max colour code expansion size */
    char *bp, buf[MAX_WRITE_SOCKET_SIZE + 16];
#endif

#ifdef COLOUR_SUPPORT
    int colour_length = 0;
    colour_parser_func colour_parser = NULL;
    int last_colour = e_no_colour;
    struct colour_data colour_data;
#endif;

    if (ob == NULL || ob->flags & O_DESTRUCTED ||
	(ip = ob->interactive) == NULL ||
	ip->do_close) {
	(void)fputs(cp, stderr);
	(void)fflush(stderr);
	return;
    }

#ifdef COLOUR_SUPPORT
    /* ip->colour_method = 2;  /**/
    colour_data.default_fg = ip->default_fg; 
    colour_data.default_bg = ip->default_bg; 
    colour_data.base_buffer = buf;
    if( ip->colour_method < 3 ) {
      colour_parser = colour_parsers[ ip->colour_method ];
      colour_blocked = 0;
    }
#endif

#ifdef SUPER_SNOOP
    if (ip->snoop_fd != -1) {
        char tbuf[MAX_WRITE_SOCKET_SIZE];
        strip_ansi( cp, tbuf, MAX_WRITE_SOCKET_SIZE );
	(void)write(ip->snoop_fd, tbuf, strlen(tbuf));
    }
#endif

    if (ip->snoop_by != NULL)
	add_message2(ip->snoop_by->ob, "%%%s", cp);

    /* ip->screen_width = 0; */
#ifdef WORD_WRAP
    if (ip->screen_width != 0)
    {
	current_column = ip->current_column;

	bp = buf;

	for (;;)
	{
	    if (*cp == '\0')
		break;

	    if (bp >= &buf[MAX_WRITE_SOCKET_SIZE])
		error("Too long message!\n");

	    if (*cp == '\n')
	    {
		current_column = 0;
#ifdef COLOUR_SUPPORT
		/** Add ^* to emtpy colour */
                if( colour_blocked || last_colour == e_colour_code ) 
                  colour( &colour_data, &bp, NULL, 0, colour_parser );
                last_colour = e_no_colour;
#endif
		*bp++ = *cp++;
		continue;
	    }

	    if (*cp == '\t')
	    {
		current_column &= ~7;
		current_column += 8;
		if (current_column >= ip->screen_width)
		{
		    current_column = 0;
		    *bp++ = '\n';
		    while (*cp == ' ' || *cp == '\t')
			cp++;
		}
		else
		{
		    *bp++ = *cp++;
		}
		continue;
	    }

	    len = 1;

#ifdef COLOUR_SUPPORT
            colour_length = 0;
            while (cp[len] > ' ' && cp[len - 1] != '-') {
                if( cp[ len ] == '^' ) {
                  switch( cp[ len + 1 ] ) {
                    case '=' :
                      len += 4;
                      colour_length += 4;
                      break;
                    case '+' : case '-':
                      len += 3;
                      colour_length += 3;
                      break;
                    case '*' : case 'n' : case 'N':
                      len += 2;
                      colour_length += 2;
                      break;
                    default:
                      len += 2;
                      colour_length += 1;
                      break;
                  }
                } else {
		  len++;
                }
            }

            len -= colour_length;
#else
            while (cp[len] > ' ' && cp[len - 1] != '-') 
		len++;
#endif

	    if (current_column + len >= ip->screen_width)
	    {
		if (current_column > ip->screen_width * 4 / 5)
		{
		    current_column = 0;
		    *bp++ = '\n';
		    while (*cp == ' ' || *cp == '\t')
			cp++;
		    if (*cp == '\n') {
#ifdef COLOUR_SUPPORT
                        /** Copy in null color code */
                        if( colour_blocked || last_colour == e_colour_code ) 
                          colour( &colour_data, &bp, NULL, 0, colour_parser );
                        last_colour = e_no_colour;
#endif
			cp++;
                    }
		    continue;
		}
	    }

	    if (bp + len >= &buf[MAX_WRITE_SOCKET_SIZE])
		error("Too long message!\n");

	    current_column += len;
#ifdef COLOUR_SUPPORT
            switch( colour( &colour_data, &bp, &cp, len, colour_parser ) ) {
              case e_no_colour: break; /* No change */
              case e_colour_cleared: last_colour = e_colour_cleared; break;
              case e_colour_code: last_colour = e_colour_code; break;
            }
#else
	    for (; len > 0; len--)
		*bp++ = *cp++;
#endif
	}

	*bp = '\0';

	ip->current_column = current_column;

	telnet_output(ip->tp, (u_char *)buf);
    }
    else
#endif
    {
        int message_length = strlen( cp );

#ifdef COLOUR_SUPPORT
        /*** THIS CODE DOESN'T HANDLE NEWLINES CORRECTLY ***/
        char *end = cp + message_length;
        int segment_length = 0;
        int is_newline = 0;

        if ( message_length > MAX_WRITE_SOCKET_SIZE)
 	    error("Too long message!\n");
        bp = buf;
        while( cp < &buf[ message_length ] ) {
          colour_length = 0;
          len = 1;
          while (cp[len] && cp[len] != '\n' && &cp[len] < end ) {
            if( cp[ len ] == '^' ) {
              switch( cp[ len + 1 ] ) {
                case '=' :
                  len += 4;
                  colour_length += 4;
                  break;
                case '+' : case '-':
                  len += 3;
                  colour_length += 3;
                  break;
                case '*' : case 'n' : case 'N':
                  len += 2;
                  colour_length += 2;
                  break;
                default:
                  len += 2;
                  colour_length += 1;
                  break;
              }
            } else {
 	      len++;
            }
          }
          if( cp[ len ] == '\n' ) 
            is_newline = 1; 
          else
            is_newline = 0;

          switch( colour( &colour_data, &bp, &cp, len - colour_length, 
                          colour_parser ) ) {
            case e_no_colour: break; /* No change */
            case e_colour_cleared: last_colour = e_colour_cleared; break;
            case e_colour_code: last_colour = e_colour_code; break;
          }

          if( is_newline ) {
            *bp++ = '\n';
            cp++;
            if( colour_blocked || last_colour == e_colour_code )
              colour( &colour_data, &bp, NULL, 0, colour_parser );
          } else {
            break;
          }
        }
        *bp = '\0';
        telnet_output( ip->tp, (u_char *)buf );
#else
        if ( message_length > MAX_WRITE_SOCKET_SIZE)
 	    error("Too long message!\n");
        telnet_output(ip->tp, (u_char *)cp);
#endif
    }
}

#define MSR_SIZE 20

int	msecs_response[MSR_SIZE];
int	msr_point		= -1;

int 
get_msecs_response(int ix)
{
    return (ix >= 0 && ix < MSR_SIZE) ? msecs_response[ix] : -1;
}

/*
 * Remove an interactive player immediately.
 */
void
remove_interactive(struct interactive *ip, int link_dead)
{
    struct object *save = command_giver;
    struct object *ob = ip->ob;
    int i;

    if (!ob || !(ob->interactive))
	return;
    for (i = 0; i < MAX_PLAYERS; i++)
    {
	if (all_players[i] != ob->interactive)
	    continue;
	if (ob->interactive->closing)
	    fatal("Double call to remove_interactive()\n");

	command_giver = ob;

	if (ob->interactive->ed_buffer)
	{
	    extern void save_ed_buffer(void);

	    /* This will call 'get_ed_buffer_save_file_name' in master_ob
             * If that fails(error in LPC) the closing IP will hang and
	     * on the next read a 'fatal read on closing socket will occur'
	     * unless we do this here before ip->closing = 1
	     */
	    (void)add_message("Saving editor buffer.\n");
	    save_ed_buffer();
	}
	if (ob->interactive == NULL)
	    return;
	ob->interactive->closing = 1;
	if (ob->interactive->snoop_by)
	{
	    ob->interactive->snoop_by->snoop_on = 0;
	    ob->interactive->snoop_by = 0;
	}
	if (ob->interactive->snoop_on)
	{
	    ob->interactive->snoop_on->snoop_by = 0;
	    ob->interactive->snoop_on = 0;
	}
	write_socket("Closing down.\n", command_giver);
	telnet_detach(ob->interactive->tp);
#ifdef SUPER_SNOOP
	if (ob->interactive->snoop_fd >= 0) {
	    (void)close(ob->interactive->snoop_fd);
	    ob->interactive->snoop_fd = -1;
	}
#endif
	num_player--;
	if (ob->interactive->input_to)
	{
	    free_sentence(ob->interactive->input_to);
	    ob->interactive->input_to = 0;
	}
	if(ob->interactive->rname)
	    free(ob->interactive->rname);
	
	free((char *)ob->interactive);
	ob->interactive = 0;
	all_players[i] = 0;
	free_object(ob, "remove_interactive");

	push_object(ob);
	push_number(link_dead);
	(void)apply_master_ob(M_REMOVE_INTERACTIVE,2);
	
	command_giver = save;
	return;
    }
    fatal("Could not find and remove player %s\n", ob->name);
}


/*
 * get the I'th player object from the interactive list, i starts at 0
 * and can go to num_player - 1.  For users(), etc.
 */
struct object * 
get_interactive_object(i)
int i;
{
    int n;

    if (i >= num_player) /* love them ASSERTS() :-) */
	fatal("Get interactive (%d) with only %d players!\n", i, num_player);

    for (n = 0; n < MAX_PLAYERS; n++)
	if (all_players[n])
	    if (!(i--))
		return(all_players[n]->ob);

    fatal("Get interactive: player %d not found! (num_players = %d)\n",
		i, num_player);
    return 0;	/* Just to satisfy some compiler warnings */
}

void *
new_player(void *tp, struct sockaddr_in *addr, size_t len)
{
    int i;

    for (i = 0; i < MAX_PLAYERS; i++)
    {
	struct object *ob;
	struct svalue *ret;
	extern struct object *master_ob;
	
	if (all_players[i] != 0)
	    continue;
	command_giver = master_ob;
	master_ob->interactive =
	    (struct interactive *)xalloc(sizeof (struct interactive));
	master_ob->interactive->default_err_message = 0;
	master_ob->flags |= O_ONCE_INTERACTIVE;
	/* This initialization is not pretty. */
	master_ob->interactive->rname = 0;
	master_ob->interactive->ob = master_ob;
	master_ob->interactive->input_to = 0;
	master_ob->interactive->closing = 0;
	master_ob->interactive->snoop_on = 0;
	master_ob->interactive->snoop_by = 0;
	master_ob->interactive->do_close = 0;
	master_ob->interactive->noecho = 0;
	master_ob->interactive->trace_level = 0;
	master_ob->interactive->trace_prefix = 0;
	master_ob->interactive->last_time = current_time;
	master_ob->interactive->ed_buffer = 0;
#ifdef SUPER_SNOOP
	master_ob->interactive->snoop_fd = -1;
#endif
#ifdef WORD_WRAP
	master_ob->interactive->current_column = 0;
	master_ob->interactive->screen_width = 0;
#endif
#ifdef COLOUR_SUPPORT
        master_ob->interactive->colour_method = 0;
        master_ob->interactive->default_bg = 0;
        master_ob->interactive->default_fg = 0;
#endif
	all_players[i] = master_ob->interactive;
	all_players[i]->tp = tp;
	set_prompt(NULL);
	(void)memcpy((char *)&all_players[i]->addr, (char *)addr, len);
	all_players[i]->rport = ntohs(addr->sin_port);
	all_players[i]->lport = port_number;
	num_player++;
	/*
	 * The player object has one extra reference.
	 * It is asserted that the master_ob is loaded.
	 */
	add_ref(master_ob, "new_player");
	ret = apply_master_ob(M_CONNECT, 0);
	if (ret == 0 || ret->type != T_OBJECT)
	{
	    remove_interactive(master_ob->interactive, 0);
	    return NULL;
	}
	/*
	 * There was an object returned from connect(). Use this as the
	 * player object.
	 */
	ob = ret->u.ob;
	ob->interactive = master_ob->interactive;
	ob->interactive->ob = ob;
	ob->flags |= O_ONCE_INTERACTIVE;
	master_ob->flags &= ~O_ONCE_INTERACTIVE;
	master_ob->interactive = 0;
	free_object(master_ob, "reconnect");
	add_ref(ob, "new_player");
	command_giver = ob;
#ifndef NO_IP_DEMON
	hname_sendreq(query_ip_number(ob),
		      all_players[i]->lport,
		      all_players[i]->rport);
#endif
#ifdef SUPER_SNOOP
	check_supersnoop(ob);
#endif
	logon(ob);
	return all_players[i];
    }
    telnet_output(tp, (u_char *)"Lpmud is full. Come back later.\n");
    return NULL;
}

int 
call_function_interactive(struct interactive *i, char *str)
{
    struct closure *func;
    
    if (!i->input_to)
	return 0;
    func = i->input_to->funct;
    /*
     * Special feature: input_to() has been called to setup
     * a call to a function.
     */
    if (!legal_closure(func))
    {
	/* Sorry, the object has selfdestructed ! */
	free_sentence(i->input_to);
	i->input_to = 0;
	return 0;
    }

    INCREF(func->ref);
    free_sentence(i->input_to);
    /*
     * We must clear this reference before the call, because
     * someone might want to set up a new input_to().
     */
    i->input_to = 0;
    push_string(str, STRING_MSTRING);
    current_object = func->funobj; /* It seems that some code relies on previous_object having a value, this will ensure that it does. */
    (void)call_var(1, func);
    free_closure(func);
    return 1;
}

int
set_call(struct object *ob, struct sentence *sent, int noecho)
{
    struct object *save = command_giver;
    if (ob == 0 || sent == 0)
	return 0;
    if (ob->interactive == 0 || ob->interactive->input_to)
	return 0;
    ob->interactive->input_to = sent;
    ob->interactive->noecho = noecho;
    command_giver = ob;
    if (noecho)
	telnet_enable_echo(ob->interactive->tp);
    command_giver = save;
    return 1;
}

void
remove_all_players()
{
    struct svalue *ret;
    struct gdexception exception_frame;
    
    exception_frame.e_exception = NULL;
    exception_frame.e_catch = 0;

    if (setjmp(exception_frame.e_context)) 
    {
	clear_state();
	(void)add_message("Anomaly in the fabric of world space.\n");
	ret = NULL;
    }
    else
    {
	exception = &exception_frame;
	eval_cost = 0;
	ret = apply_master_ob(M_START_SHUTDOWN, 0);
    }
    if (ret && ret->type == T_ARRAY)
    {
	volatile int i;
	struct vector *unload_vec = ret->u.vec;
	
	INCREF(unload_vec->ref);
	
	for (i = 0; i < unload_vec->size; i++)
	    if (setjmp(exception_frame.e_context)) 
	    {
		clear_state();
		(void)add_message("Anomaly in the fabric of world space.\n");
	    }
	    else
	    {
		exception = &exception_frame;
		push_svalue(&(unload_vec->item[i]));
		eval_cost = 0;
		(void)apply_master_ob(M_CLEANUP_SHUTDOWN, 1);
	    }
	free_vector(unload_vec);
    }
    if (setjmp(exception_frame.e_context)) 
    {
	clear_state();
	(void)add_message("Anomaly in the fabric of world space.\n");
    }
    else
    {
	exception = &exception_frame;
	eval_cost = 0;
	ret = apply_master_ob(M_FINAL_SHUTDOWN, 0);
    }
    exception = NULL;
}

void
set_prompt(char *str)
{
    if (str)
	command_giver->interactive->prompt = str;
    else
	command_giver->interactive->prompt = "> ";
}

/*
 * Print the prompt, but only if input_to not is disabled.
 */
void
print_prompt()
{
    if (command_giver == 0)
	fatal("command_giver == 0.\n");
    if (command_giver->interactive->input_to == 0)
    {
	if (!(command_giver->flags & O_DESTRUCTED) &&
	    command_giver->interactive->prompt)
	    (void)add_message("%s", command_giver->interactive->prompt);
	/* add test for heart_beat later */
    }
}


/*
 * Let object 'me' snoop object 'you'. If 'you' is 0, then turn off
 * snooping.
 *
 * This routine is almost identical to the old set_snoop. The main
 * difference is that the routine writes nothing to player directly,
 * all such communication is taken care of by the mudlib. It communicates
 * with master.c in order to find out if the operation is permissble or
 * not. The old routine let everyone snoop anyone. This routine also returns
 * 0 or 1 depending on success.
 */
int
set_snoop(struct object *me, struct object *you)
{
    struct interactive *on = 0, *by = 0, *tmp;
    int i;
    struct svalue *ret;
    extern struct object *master_ob;

    
    /* Stop if people managed to quit before we got this far */
    if (me->flags & O_DESTRUCTED)
	return 0;
    if (you && (you->flags & O_DESTRUCTED))
	return 0;
    
    /* Find the snooper & snopee */
    for(i = 0 ; i < MAX_PLAYERS && (on == 0 || by == 0); i++) 
    {
	if (all_players[i] == 0)
	    continue;
	if (all_players[i]->ob == me)
	    by = all_players[i];
	else if (all_players[i]->ob == you)
	    on = all_players[i];
    }

    /* Check for permissions with valid_snoop in master */
    if (current_object != master_ob)
    {
	push_object(current_object);
	push_object(me);
	if (you == 0)
	    push_number(0);
	else
	    push_object(you);
	ret = apply_master_ob(M_VALID_SNOOP, 3);
	
	if (ret && (ret->type != T_NUMBER || ret->u.number == 0))
	    return 0;
    }
    /* Stop snoop */
    if (you == 0) 
    {
	if (by == 0)
	    error("Could not find snooper to stop snoop on.\n");
	if (by->snoop_on == 0)
	    return 1;
	by->snoop_on->snoop_by = 0;
	by->snoop_on = 0;
	return 1;
    }

    /* Strange event, but possible, so test for it */
    if (on == 0 || by == 0)
	return 0;

    /* Protect against snooping loops */
    for (tmp = on; tmp; tmp = tmp->snoop_on) 
    {
	if (tmp == by) 
	    return 0;
    }

    /* Terminate previous snoop, if any */
    if (by->snoop_on) 
    {
	by->snoop_on->snoop_by = 0;
	by->snoop_on = 0;
    }
    if (on->snoop_by)
    {
	on->snoop_by->snoop_on = 0;
	on->snoop_by = 0;
    }

    on->snoop_by = by;
    by->snoop_on = on;
    return 1;
    
}

#define IPSIZE 200
static struct ipentry
{
    unsigned long addr;
    char *name;
} iptable[IPSIZE];
static int ipcur;

#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN
void
clear_ip_table()
{
    int i;

    for (i = 0; i < IPSIZE; i++)
	if (iptable[i].name) {
	    free_sstring(iptable[i].name);
	    iptable[i].name = NULL;
	}
}
#endif

char *
query_ip_name(struct object *ob)
{
    int i;

    if (ob == 0)
	ob = command_giver;
    if (!ob || ob->interactive == 0)
	return 0;
    for(i = 0; i < IPSIZE; i++)
    {
	if (iptable[i].addr ==
	    (ob->interactive->addr.sin_addr.s_addr & 0xffffffff) &&
	    iptable[i].name)
	    return iptable[i].name;
    }
    return inet_ntoa(ob->interactive->addr.sin_addr);
}

void
add_ip_entry(unsigned long addr, char *name)
{
    int i;

    for(i = 0; i < IPSIZE; i++)
    {
	if (iptable[i].addr == addr)
	    return;
    }
    iptable[ipcur].addr = addr;
    if (iptable[ipcur].name)
	free_sstring(iptable[ipcur].name);
    iptable[ipcur].name = make_sstring(name);
    ipcur = (ipcur+1) % IPSIZE;
}

char *
query_ip_number(struct object *ob)
{
    if (ob == 0)
	ob = command_giver;
    /**printf( "Name is %s\n",  ob->name ); 
    printf( "Living Name is %s\n",  ob->living_name ); **/
    if (!ob || ob->interactive == 0)
	return 0;
    return inet_ntoa(ob->interactive->addr.sin_addr);
}

#ifndef INET_NTOA_OK
char *
inet_ntoa(struct in_addr in)
{
    static char b[18];
    char *p;

    p = (char *)&in;
#define	UC(b)	(((int)b) & 0xFF)
    (void)sprintf(b, "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
#undef UC
    return b;
}
#endif /* INET_NTOA_OK */

char *
query_host_name()
{
    static char name[20];
    
    (void)gethostname(name, sizeof name);
    name[sizeof name - 1] = '\0';	/* Just to make sure */
    return name;
}

struct object *
query_snoop(struct object *ob)
{
    if (ob->interactive->snoop_by == 0)
	return 0;
    return ob->interactive->snoop_by->ob;
}

int
query_idle(struct object *ob)
{
    if (!ob->interactive)
	error("query_idle() of non-interactive object.\n");
    return current_time - ob->interactive->last_time;
}

void
notify_no_command()
{
    char *p,*m;
    extern struct object *vbfc_object;

    if (!command_giver->interactive)
	return;
    p = command_giver->interactive->default_err_message;
    if (p) 
    {
	/* We want 'value by function call' 
	 */
	m = process_string(p, vbfc_object != 0); 
	(void)add_message("%s", m);
	if (m != p)
	    free(m);
	free_sstring(p);
	command_giver->interactive->default_err_message = 0;
    }
    else
	(void)add_message("What ?\n");
}

void 
clear_notify()
{
    if (!command_giver || !command_giver->interactive)
	return;
    if (command_giver->interactive->default_err_message)
    {
	free_sstring(command_giver->interactive->default_err_message);
	command_giver->interactive->default_err_message = 0;
    }
}

void
set_notify_fail_message(char *str, int pri)
{
    if (!command_giver || !command_giver->interactive)
	return;
    if (command_giver->interactive->default_err_message && pri == 0)
	return;
    clear_notify();
    if (command_giver->interactive->default_err_message)
	free_sstring(command_giver->interactive->default_err_message);
    command_giver->interactive->default_err_message = make_sstring(str);
}

int 
replace_interactive(struct object *ob, struct object *obfrom,
		    /*IGN*/char *name)
{
    struct svalue *v;

    /*
     * Check with master that exec allowed
     */
    push_string(name, STRING_MSTRING);
    if (ob)
	push_object(ob);
    else
	push_number(0);
    push_object(obfrom);
    v = apply_master_ob(M_VALID_EXEC, 3);
    if (v && (v->type != T_NUMBER || v->u.number == 0))
	return 0;

    /* (void)fprintf(stderr,"DEBUG: %s,%s\n",ob->name,obfrom->name); */
    if (ob && ob->interactive)
	error("Bad argument1 to exec(), was an interactive\n");
    if (!obfrom->interactive)
	error("Bad argument2 to exec(), was not an interactive\n");
    if (ob)
    {
#ifdef SUPER_SNOOP
	if (obfrom->interactive->snoop_fd >= 0) {
	    (void)close(obfrom->interactive->snoop_fd);
	    obfrom->interactive->snoop_fd = -1;
	}
#endif
	ob->interactive = obfrom->interactive;
	ob->interactive->ob = ob;
	ob->flags |= O_ONCE_INTERACTIVE;
	add_ref(ob, "exec");
	free_object(obfrom, "exec");
	obfrom->interactive = 0;
	obfrom->flags &= ~O_ONCE_INTERACTIVE;
#ifdef SUPER_SNOOP
	check_supersnoop(ob);
#endif
#ifdef HIDE_HOST_HACK
        set_hidden_ip( ob );
#endif
    }
    else
	remove_interactive(obfrom->interactive, 0);
    if (obfrom == command_giver) command_giver = ob;
    return 1;
}

#ifdef DEBUG
/*
 * This is used for debugging reference counts.
 */

void
update_ref_counts_for_players()
{
    int i;

    for (i = 0; i<MAX_PLAYERS; i++)
    {
	if (all_players[i] == 0)
	    continue;
	all_players[i]->ob->extra_ref++;
	if (all_players[i]->input_to)
	    all_players[i]->input_to->funct->funobj->extra_ref++;
    }
}
#endif /* DEBUG */

void
interactive_input(struct interactive *ip, char *cp)
{
    extern void print_mudstatus(char *, int, int, int);
    extern void reset_mudstatus(void);
    extern void update_load_av(void);
    extern int get_processtime(void);
    extern int get_millitime(void);
    extern void ed_cmd(char *);
    struct gdexception exception_frame;

#ifdef WORD_WRAP
    ip->current_column = 0;
#endif

    if (ip->noecho)
    {
	ip->noecho = 0;
	telnet_disable_echo(ip->tp);
    }
    else
    {
#ifdef SUPER_SNOOP
	if (ip->snoop_fd != -1)
	{
	    (void)write(ip->snoop_fd, cp, strlen(cp));
	    (void)write(ip->snoop_fd, "\n", 1);
	}
#endif

	if (ip->snoop_by != NULL)
	    add_message2(ip->snoop_by->ob, "%% %s\n", cp);
    }

    ip->last_time = current_time;

    update_load_av();

    /*
     * Now we have a string from the player. This string can go to
     * one of several places. If it is prepended with a '!', then
     * it is an escape from the 'ed' editor, so we send it as a
     * command to the parser.
     *
     * If any object function is waiting for an input string, then
     * send it there.
     *
     * Otherwise, send the string to the parser.
     *
     * The player_parser() will find that current_object is NULL,
     * and then set current_object to point to the object that
     * defines the command. This will enable such functions to be
     * static.
     */
    command_giver = ip->ob;
    current_object = NULL;
    current_interactive = command_giver;
    previous_ob = command_giver;

#ifdef DEBUG
    if (!command_giver->interactive)
	fatal("Non interactive player in main loop!\n");
#endif

    if (s_flag)
    {
	reset_mudstatus();
    }

    exception_frame.e_exception = exception;
    exception_frame.e_catch = 0;
    exception = &exception_frame;

    if (setjmp(exception_frame.e_context) == 0)
    {
	if (cp[0] == '!' && command_giver->super)
	    (void)parse_command(cp + 1, command_giver);
	else
	    if (command_giver->interactive->ed_buffer)
		ed_cmd(cp);
	    else
		if (!call_function_interactive(command_giver->interactive, cp))
		    (void)parse_command(cp, command_giver);
    }

    exception = exception_frame.e_exception;

    if (command_giver != NULL && command_giver->interactive != NULL)
    {
	print_prompt();

	if (s_flag)
	{
	    print_mudstatus(command_giver->name, eval_cost, get_millitime(), get_processtime());
	}
    }
}    
