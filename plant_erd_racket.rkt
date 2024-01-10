#lang racket

(require db)
(require racket/cmdline)

(define server  (vector-ref (current-command-line-arguments) 0))
(define database (vector-ref (current-command-line-arguments) 1))
(define user (vector-ref (current-command-line-arguments) 2))
(define password (vector-ref (current-command-line-arguments) 3))

(define pgc
  (postgresql-connect #:server server 
                      #:user user
                      #:database database
                      #:password password))

(struct table (name column foreign-key indexes))
(struct column (name type nullable primary))

(define (make-table tb)
  (table (list-ref tb 0)
         (list-ref tb 1)
         (list-ref tb 2)
         (list-ref tb 3)))

(define (make-column row pkeys)
  (column (vector-ref row 0)
          (vector-ref row 1)
          (vector-ref row 2)
          (if (and (vector? pkeys) (vector-member (vector-ref row 0) pkeys)) #t #f)
          ))

(define (make-columns rows pkeys)
  (map (lambda (row)
         (make-column row pkeys))
         rows))

(define (get-all-table-names pgc)
  (query-list pgc
              "SELECT relname FROM pg_stat_user_tables where schemaname = 'public' ORDER BY relname"))

(define (get-table-columns pgc table)
  (query-rows pgc
              "SELECT column_name,
           data_type,
	   is_nullable
     FROM information_schema.columns
     WHERE table_name = $1
     order by ordinal_position" table))

(define (get-primary-key-columns pgc table)
  (query-maybe-row pgc
              "SELECT ccu.column_name as COLUMN_NAME
		FROM information_schema.table_constraints tc,
		     information_schema.constraint_column_usage ccu
		WHERE tc.table_name=$1
		AND tc.constraint_type='PRIMARY KEY'
		AND tc.table_catalog=ccu.table_catalog
		AND tc.table_schema=ccu.table_schema
		AND tc.table_name=ccu.table_name
		AND tc.constraint_name=ccu.constraint_name" table))

(define (get-table tb)
  (make-table (list tb (make-columns (get-table-columns pgc tb) (get-primary-key-columns pgc tb)) null null)))

(define (load-schema)
  (let ([tables (get-all-table-names pgc)])
    (map get-table tables)))

(define (print-table-schema tb)
(printf "entity ~a {\n" (table-name tb))
  (for* ([clm (table-column tb)])
    (if (eq? (column-primary clm) #t)
        (printf "  + ~a: [PK]\n  ==\n" (column-name clm))
        (if (string=? (column-nullable clm) "NO")
            (printf "  *~a: ~a\n" (column-name clm) (column-type clm))
            (printf "  ~a: ~a\n" (column-name clm) (column-type clm)))
        )
    )
(printf "}\n\n"))

(let ([schema (load-schema)])
  (for* ([table schema])
    (print-table-schema table)))
  