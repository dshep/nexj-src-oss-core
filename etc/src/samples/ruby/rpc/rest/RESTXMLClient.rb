#!/usr/bin/ruby

require 'wadl'

person = WADL::Application.from_wadl(open('nexj.wadl')).Person
person = person.with_basic_auth('nexjsa', 'nexj')
read_args = { :attributes => '(fullName (addrs fullName))',
              :where      => '(and (= (@ firstName) "SOAP") (= (@ lastName) "Box"))',
              :orderBy    => '()',
              :count      => -1,
              :offset     => 0 }
response = person.get(:query => read_args)
results = response.representation.elements.to_a('item')

puts "Read #{results.length} instance(s) of Person"

for i in (0..results.length - 1)
   puts "[#{i}]: #{results[i].elements["fullName"].text}"
   puts "   addrs: "

   results[i].elements.each('addrs') do |addr|
      puts "      #{addr.elements["fullName"].text}"
   end
end
