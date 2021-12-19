package org.im97mori.ble.android.peripheral.module;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.utils.Utils;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;

@Module
public class ApplicationModule {

    @Provides
    @Singleton
    public Gson createGsonInstance() {
        return Utils.createGsonInstance();
    }

}
