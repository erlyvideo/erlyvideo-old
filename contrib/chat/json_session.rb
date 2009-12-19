require 'openssl'
require 'rack/request'
require 'rack/response'
require 'active_support'

if defined?(ActionController::Flash::FlashHash)
  class ActionController::Flash::FlashHash
    def to_json(*args)
      {}.merge(self).merge({:used => @used}).to_json
    end
  end
end

module Rack

  module Session

    # Rack::Session::Cookie provides simple cookie based session management.
    # The session is a Ruby Hash stored as base64 encoded marshalled data
    # set to :key (default: rack.session).
    # When the secret key is set, cookie data is checked for data integrity.
    #
    # Example:
    #
    #     use Rack::Session::JsonSession, :key => 'rack.session',
    #                                :domain => 'foo.com',
    #                                :path => '/',
    #                                :expire_after => 2592000,
    #                                :secret => 'change_me'
    #
    #     All parameters are optional.

    class JsonSession < Cookie
      
      def self.pack_session(session, secret)
        data = [session.to_json].pack("m*").strip
        "#{data}--#{generate_hmac(data, secret)}"
      end
      
      def self.generate_hmac(data, secret)
        OpenSSL::HMAC.hexdigest(OpenSSL::Digest::SHA1.new, secret, data)
      end

      def load_session(env)
        request = Rack::Request.new(env)
        session_data = request.cookies[@key]

        if @secret && session_data
          session_data, digest = session_data.split("--")
          session_data = nil  unless digest == generate_hmac(session_data)
        end

        begin
          session = ActiveSupport::JSON.decode(session_data.unpack("m*").first)
          
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
          
          env["rack.session"] = session_data
        rescue
          env["rack.session"] = Hash.new
        end

        env["rack.session.options"] = @default_options.dup
      end
      
      def generate_hmac(data)
        self.class.generate_hmac(data, @secret)
      end
      

      def commit_session(env, status, headers, body)
        session_data = self.class.pack_session(env["rack.session"], @secret)

        if session_data.size > (4096 - @key.size)
          env["rack.errors"].puts("Warning! Rack::Session::Cookie data size exceeds 4K. Content dropped.")
          [status, headers, body]
        else
          options = env["rack.session.options"]
          cookie = Hash.new
          cookie[:value] = session_data
          cookie[:expires] = Time.now + options[:expire_after] unless options[:expire_after].nil?
          response = Rack::Response.new(body, status, headers)
          response.set_cookie(@key, cookie.merge(options))
          response.to_a
        end
      end
    end
  end
end
