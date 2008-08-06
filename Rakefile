require 'rake/clean'

SRC = FileList['src/*.erl']

OBJDIR = 'ebin'
TMPDIR = "tmp"

OBJ = SRC.map { |filename| File.join(OBJDIR, File.basename(filename).ext('beam')) }
COV = SRC.map { |filename| File.join(COVERDIR, File.basename(filename).ext('beam')) }
CLEAN.include(OBJ, OBJDIR, TMPDIR, COVERDIR)

directory TMPDIR

directory OBJDIR

task :default => [:build]

task :build => [:eunit, LOGDIR] + OBJ

rule '.beam' => lambda{ |beamfile| find_source(beamfile) } do |t|
  if t.name =~ /^ebin/
    Task[OBJDIR].invoke
    sh "erlc -pa 'vendor/eunit/ebin' -o ebin -W #{t.source}"
  elsif t.name =~ /^cover/
    cover[COVERDIR].invoke
    sh "erlc -pa 'vendor/eunit/ebin' -o cover -W +debug_info #{t.source}"
  end
end

# helper methods

def eunit(mods)
  cmd = "erl -noshell -pa 'vendor/eunit/ebin' -pa ./ebin " + mods.map { |mod| "-s #{mod} test " }.join + "-s init stop"
  puts cmd
  test_output = `#{cmd}`  
  if /\*failed\*/ =~ test_output
    puts test_output
    raise "Tests failed"
  else
    puts test_output
  end
end

def find_source(beam)
  base = File.basename(beam, '.beam')
  SRC.find { |s| File.basename(s, '.erl') == base }
end
