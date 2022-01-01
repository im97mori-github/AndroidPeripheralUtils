package org.im97mori.ble.android.peripheral.hilt.repository;

import android.content.Context;

import androidx.annotation.NonNull;

import java.util.function.Consumer;

import javax.inject.Inject;

import io.reactivex.rxjava3.functions.Action;

public class FakeBluetoothSettingRepository extends BluetoothSettingRepository {

    public Action mAddBluetoothStatusConsumerAction;

    public Action mRemoveBluetoothStatusConsumerAction;

    @Inject
    public FakeBluetoothSettingRepository(@NonNull Context context) {
        super(context);
    }

    @Override
    public void addBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        if (mAddBluetoothStatusConsumerAction != null) {
            try {
                mAddBluetoothStatusConsumerAction.run();
            } catch (Throwable throwable) {
                throwable.printStackTrace();
            }
        }
        super.addBluetoothStatusConsumer(consumer);
    }

    @Override
    public void removeBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        if (mRemoveBluetoothStatusConsumerAction != null) {
            try {
                mRemoveBluetoothStatusConsumerAction.run();
            } catch (Throwable throwable) {
                throwable.printStackTrace();
            }
        }
        super.removeBluetoothStatusConsumer(consumer);
    }

}
