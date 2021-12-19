package org.im97mori.ble.android.peripheral;

import android.app.Application;

import org.im97mori.ble.android.peripheral.component.ApplicationComponent;
import org.im97mori.ble.android.peripheral.component.DaggerApplicationComponent;
import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.datasource.ResourceTextSource;
import org.im97mori.ble.android.peripheral.module.ApplicationModule;
import org.im97mori.ble.android.peripheral.module.DeviceModule;
import org.im97mori.ble.android.peripheral.module.TextModule;


public class AndroidPeripheralUtilsApplication extends Application {
    protected ApplicationComponent mApplicationComponent;

    @Override
    public void onCreate() {
        super.onCreate();
        mApplicationComponent = DaggerApplicationComponent
                .builder()
                .applicationModule(new ApplicationModule())
                .deviceModule(new DeviceModule(new DeviceDataSource(this)))
                .textModule(new TextModule(new ResourceTextSource(this)))
                .build();
//        mApplicationComponent.inject(this);
    }

    public ApplicationComponent getComponent() {
        return mApplicationComponent;
    }
}
