require 'rubygems'
require 'open4'
require 'rake/clean'
require "rake/classic_namespace"

include Open4

SRC = FileList['src/*.erl']

OBJDIR = 'ebin'
TMPDIR = "tmp"

OBJ =  SRC.map { |filename| File.join(OBJDIR, File.basename(filename).ext('beam')) }
CLEAN.include(OBJ, OBJDIR, TMPDIR, "erl_crash.dump")

directory TMPDIR

directory OBJDIR

task :default => [:build]

task :build => OBJ

if ENV["DEBUG"]
  puts "Compiling with debug_info (used for coverage as well)"
  debug_flag = "+debug_info"
else 
  debug_info = ""
end

rule '.beam' => lambda{ |beamfile| find_source(beamfile) } do |t|
  if t.name =~ /^ebin/
    Task[OBJDIR].invoke
    sh "erlc -o #{OBJDIR} #{debug_flag} -W #{t.source}"
  else
    raise("Don't know how to build '#{t.name}'")
  end
end

task :graph => [:build] do
  sh "erl -noshell -pa ebin -s hardwood t1 -s init stop"
  sh "cat hardwood.gv | dot -Tpng -o btree.png"
  sh "open -g btree.png"
end

task :test => OBJ do
  test(OBJ)
end

# helper methods

# print lines from buffer using prefix before each
def plines(buffer, prefix="  *")
  prefix ||= ""
  buffer.lines.each do |line|
    puts prefix + " " + line
  end
end

def test(mods)
  cmd = "erl -noshell -pa ./ebin " + mods.map { |mod| "-s #{File.basename(mod, ".beam")} test " }.join + "-s init stop"
  puts cmd
  popen4(cmd) do |pid, stdin, stdout, stderr|
    out = stdout.read 
    err = stderr.read
    if /badmatch/ =~ out
      plines(out)
      abort "[Tests failed]"
    else
      plines(out)
      plines(err, " **")
    end
  end
end

def find_source(beam)
  base = File.basename(beam, '.beam')
  SRC.find { |s| File.basename(s, '.erl') == base }
end
