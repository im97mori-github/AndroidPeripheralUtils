package org.im97mori.ble.android.peripheral.room;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;

import android.content.Context;
import android.os.Build;

import androidx.room.Room;
import androidx.room.util.TableInfo;
import androidx.sqlite.db.SupportSQLiteDatabase;
import androidx.sqlite.db.SupportSQLiteOpenHelper;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class AppDatabaseTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_00001() {
        AppDatabase appDatabase = Room.inMemoryDatabaseBuilder(mContext, AppDatabase.class)
                .build();
        try (SupportSQLiteOpenHelper sqLiteOpenHelper = appDatabase.getOpenHelper()) {
            SupportSQLiteDatabase sqLiteDatabase = sqLiteOpenHelper.getReadableDatabase();
            assertEquals(1, sqLiteDatabase.getVersion());

            TableInfo tableInfo = TableInfo.read(sqLiteDatabase, "device_setting");
            TableInfo.Column column = tableInfo.columns.get("device_setting_id");
            assertNotNull(column);
            assertTrue(column.notNull);
            assertEquals("INTEGER", column.type);

            column = tableInfo.columns.get("device_setting_name");
            assertNotNull(column);
            assertTrue(column.notNull);
            assertEquals("TEXT", column.type);

            column = tableInfo.columns.get("device_type");
            assertNotNull(column);
            assertTrue(column.notNull);
            assertEquals("INTEGER", column.type);

            column = tableInfo.columns.get("device_setting_data");
            assertNotNull(column);
            assertFalse(column.notNull);
            assertEquals("BLOB", column.type);
        }
    }
}