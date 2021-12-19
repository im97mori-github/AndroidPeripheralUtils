package org.im97mori.ble.android.peripheral.ui.main;

import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.GridView;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceListLauncherContract;

import java.util.Collections;
import java.util.LinkedList;

import dagger.hilt.android.AndroidEntryPoint;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

@AndroidEntryPoint
public class MainActivity extends BaseActivity {

    private MainViewModel mViewModel;

    private DeviceListAdapter adapter;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
    });

    private final ActivityResultLauncher<Void> mStartDeviceTypeListActivity = registerForActivityResult(new DeviceListLauncherContract(), result -> {
        if (result != null) {
            mStartDeviceSettingActivity.launch(Pair.create(DeviceSettingLauncherContract.UNSAVED_DEVICE_ID, result));
        }
    });

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(MainViewModel.class);

        setContentView(R.layout.main_activity);

        GridView gridView = findViewById(R.id.grid);
        gridView.setEmptyView(findViewById(R.id.empty));
        adapter = new DeviceListAdapter(this, Collections.synchronizedList(new LinkedList<>()));
        gridView.setAdapter(adapter);
        gridView.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent(getApplicationContext(), PeripheralActivity.class);
            intent.putExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, adapter.getItemId(position));
            startActivity(intent);
        });

        MaterialToolbar bar = findViewById(R.id.topAppBar);
        bar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.getDeviceList()
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(devices -> {
                            adapter.setDeviceList(devices);
                            findViewById(R.id.rootContainer).setVisibility(View.VISIBLE);
                        }
                        , Throwable::printStackTrace));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (R.id.create_device == item.getItemId()) {
            mStartDeviceTypeListActivity.launch(null);
            result = true;
        } else if (R.id.clear_devices == item.getItemId()) {
            mDisposable.add(mViewModel.deleteAllDevices()
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe());
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}