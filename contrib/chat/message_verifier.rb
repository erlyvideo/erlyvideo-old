# This is a monkey patch for MessageVerifier — I need to change Marshal.load to JSON

if defined?(ActionController::Flash::FlashHash)
  class ActionController::Flash::FlashHash
    def to_json(*args)
      {}.merge(self).merge({:used => @used}).to_json
    end
  end
end


module ActiveSupport
  class MessageVerifier
    def verify(signed_message)
      data, digest = signed_message.split("--")
      if digest != generate_digest(data)
        raise InvalidSignature
      else
        session = ActiveSupport::JSON.decode(ActiveSupport::Base64.decode64(data))

        flash = nil
        if session["flash"] && session["flash"].is_a?(Hash)
          session_flash = session.delete("flash")
          session_flash.symbolize_keys!
          used = session_flash.delete(:used)
          flash = ActionController::Flash::FlashHash.new.update(session_flash)
          if used
            used.symbolize_keys!
            flash.instance_variable_set("@used", used)
          end
        end
        session.symbolize_keys!
        session["flash"] = flash if flash

        session
      end
    rescue Exception
      {}
    rescue ActiveSupport::JSON::ParseError
      # raise InvalidSignature
      {}
    end
    
    def generate(value)
      data = ActiveSupport::Base64.encode64s(value.to_json)
      "#{data}--#{generate_digest(data)}"
    end
  end
end
