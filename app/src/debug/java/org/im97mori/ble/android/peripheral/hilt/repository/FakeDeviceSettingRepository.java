package org.im97mori.ble.android.peripheral.hilt.repository;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.CompletableOnSubscribe;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceSettingRepository extends DeviceSettingRepository {

    public PublishProcessor<List<DeviceSetting>> mLoadAllDeviceSettingProcessor;

    public PublishProcessor<DeviceSetting> mLoadDeviceSettingByIdProcessor;

    public CompletableOnSubscribe mDeleteAllDeviceSettingSubscribe;

    public Consumer<DeviceSetting> mDeleteDeviceSettingConsumer;

    public String mGetDeviceSettingNameErrorString;

    public Consumer<DeviceSetting> mInsertDeviceSettingConsumer;

    @Inject
    public FakeDeviceSettingRepository(@NonNull DeviceSettingDataSource deviceSettingDataSource
            , @NonNull @ApplicationContext Context context) {
        super(deviceSettingDataSource, context);
    }

    @NonNull
    @Override
    public Flowable<List<DeviceSetting>> loadAllDeviceSetting() {
        return Flowable.fromPublisher(mLoadAllDeviceSettingProcessor);
    }

    @Override
    public Single<DeviceSetting> loadDeviceSettingById(long id) {
        return Single.fromPublisher(mLoadDeviceSettingByIdProcessor);
    }

    @NonNull
    @Override
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(emitter -> {
            mInsertDeviceSettingConsumer.accept(deviceSetting);
            emitter.onComplete();
        });
    }

    @NonNull
    @Override
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return Completable.create(emitter -> {
            mDeleteDeviceSettingConsumer.accept(deviceSetting);
            emitter.onComplete();
        });
    }

    @NonNull
    @Override
    public Completable deleteAllDeviceSetting() {
        return Completable.create(mDeleteAllDeviceSettingSubscribe);
    }

    @Nullable
    @Override
    public String getDeviceSettingNameErrorString(@Nullable CharSequence charSequence) {
        if (mGetDeviceSettingNameErrorString == null) {
            return super.getDeviceSettingNameErrorString(charSequence);
        } else {
            return mGetDeviceSettingNameErrorString;
        }
    }

}
