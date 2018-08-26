function _precompile_()
    precompile(Tuple{typeof(Rsvg.__init__)})
    precompile(Tuple{typeof(Rsvg.handle_new_from_file), String, Rsvg.GError})
    precompile(Tuple{typeof(Rsvg.destroy), Rsvg.RsvgHandle})
end
