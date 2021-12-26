package org.im97mori.ble.android.peripheral.hilt.repository;

import android.content.Context;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.CompletableOnSubscribe;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceRepository extends DeviceRepository {

    public PublishProcessor<List<DeviceSetting>> mLoadDevicesProcessor;

    public CompletableOnSubscribe mCompletableOnSubscribe;

    @Inject
    public FakeDeviceRepository(@NonNull DeviceDataSource deviceDataSource, @NonNull @ApplicationContext Context context) {
        super(deviceDataSource, context);
    }

    @NonNull
    @Override
    public Flowable<List<DeviceSetting>> loadAllDeviceSettings() {
        return mLoadDevicesProcessor;
    }

    @NonNull
    @Override
    public Completable deleteAllDeviceSettings() {
        return Completable.create(mCompletableOnSubscribe);
    }

}
