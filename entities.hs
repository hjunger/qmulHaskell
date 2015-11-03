module Entites where

data Name = {
				first::String,
				last::String
			}

data Friend = {
				id::String,
				name::String
			}

data User = User {
					_id::String, 
					index::Int, 
					isActive::Bool,
					balance::String,
					picture::String, 
					age::Int,
					eyeColor::String,
					name::Name,
					company::String,
					email::String,
					phone::String,
					address::String,
					registered::String
					latitude::String,
					longitude::String,
					friends::[Friend],
					greeting::String,
					favoriteFruit::String

				} deriving (Read,  Show, Enum, Eq, Ord)










